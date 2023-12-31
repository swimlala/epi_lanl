CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-01-15T06:31:28Z AOML 3.0 creation; 2016-06-01T00:08:22Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150115063128  20160531170822  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               gA   AO  4055_7112_103                   2C  D   APEX                            5374                            041511                          846 @�2�0[ 1   @�2����@9�(�\�dOƧ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    gA   A   A   @���@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy��D�fD�P D�� D���D�	�D�@ D��fD��3D�� D�FfD��3D��3D�3D�@ D�p D��fD�	�D�9�D�ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�
=A�A'�AG�Ag�A�A�A�A�A�A�A�A�B�HB	�HB�HB�HB!�HB)�HB1�HB9�HBA�HBI�HBQ�HBY�HBa�HBi�HBq�HBy�HB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C xRCxRCxRCxRCxRC
xRCxRCxRCxRCxRCxRCxRCxRCxRCxRCxRC xRC"xRC$xRC&xRC(xRC*xRC,xRC.xRC0xRC2xRC4xRC6xRC8xRC:xRC<xRC>xRC@xRCBxRCDxRCFxRCHxRCJxRCLxRCNxRCPxRCRxRCTxRCVxRCXxRCZxRC\xRC^xRC`xRCbxRCdxRCfxRChxRCjxRClxRCnxRCpxRCrxRCtxRCvxRCxxRCzxRC|xRC~xRC�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�/\C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt��Dy׮D�%pD�_
D��
D�ȤD��D�O
D��pD��=D��
D�UpD��=D��=D�"=D�O
D�
D��pD��D�H�D�up1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��wA��wA��wA�A�ƨA�ĜA�ȴA���A���A���A���A���A���A�ȴA�A�ĜA��wA��FA��uA�p�A�;dA���A��wA���A�jA�&�A�+A�{A���A���A�G�A��A��-A��hA�XA��A��;A�A��-A��\A�hsA�M�A�"�A��;A���A��DA�v�A�G�A�5?A��wA�z�A�G�A��A��#A���A�t�A�v�A�x�A�~�A�|�A�l�A�v�A�ffA�n�A�z�A��A�|�A���A��
A�ZA�VA�oA��;A���A�bA���A��9A���A�O�A�/A��A���A�|�A���A�ƨA���A�jA�JA���A�33A��FA�A�A�A��!A�"�A���A��/A�ZA��`A�dZA��^A�A�E�AƨA~�9A}VA{��A{G�Az��AzbNAzAx�Aw�-Av�Av1'Au;dAt �Ar��Ao�Aj��Aj�Ai��Ah��Af{Ab�`AbjAb5?Aax�A_"�A]�A\ffA[7LAXĜAV��AVI�AV9XAV{AU��AU�-AU�AUl�AUt�AUO�AR1AOC�ANn�AM�#AMK�AJA�AHA�AF��AE�ADn�AC��ACVABI�AA�A>ȴA=�A<�A<��A;+A97LA7;dA6A�A5�mA5��A5;dA45?A3��A3�A3%A2��A2�uA2=qA1��A0ZA.��A-��A,1'A*{A'�A&ZA&�A&  A%�A%��A%�wA%��A%�A%&�A$~�A#��A r�A{Ap�A
=A�yA��A��AQ�A�A|�A~�A�7A��A��A��AVA�AA��A�/A5?A�A�mAl�A��A��A
bA	G�A��A=qAx�A�HAQ�A��A/A��AJA��A5?A�;A�hA%A �jA Z@��@��@�=q@���@��9@���@�|�@�"�@��7@�hs@�hs@�%@��D@��;@�-@�dZ@�@���@�bN@�  @��@�@�M�@�h@땁@�
=@�x�@��@�R@�+@���@�z�@��y@ᙚ@�o@�~�@�b@�-@�@���@ҸR@�  @�v�@�p�@��m@�X@��m@�5?@�@�X@��/@�z�@�|�@�n�@���@�|�@���@��7@���@��@�E�@�X@�A�@�@���@��@�`B@�Ĝ@��F@�"�@��+@��#@��D@�\)@�~�@��@�`B@��`@���@�Z@�1@���@�\)@��@��+@�5?@�x�@�%@��/@�Q�@�"�@��@�7L@��/@�r�@��@�@�^5@�x�@��j@��D@���@���@�dZ@��@�ȴ@��!@�-@��7@�/@���@�bN@�(�@�dZ@���@�@��#@�hs@�7L@��@��@�Z@�K�@���@�J@��-@�z�@��F@��@�t�@�dZ@��@��!@�5?@��7@�z�@�l�@�~�@�E�@�$�@�p�@���@�r�@�A�@���@���@�t�@�dZ@�K�@�;d@�o@�@��y@��@���@��+@�$�@��@��-@���@���@�hs@�O�@�&�@��@�V@�%@���@���@���@��D@�z�@�z�@�bN@�A�@� �@��@���@�;d@�33@�o@���@��R@��R@��\@�5?@���@��@��@��^@�X@��@���@���@�1'@�dZ@��@��H@��@���@���@�ȴ@�ȴ@�ȴ@�ȴ@���@�~�@�M�@�J@��@��-@���@�hs@�7L@�%@���@���@��@�Ĝ@��u@��@�r�@�I�@�I�@�(�@�  @l�@
=@~ȴ@~5?@}�T@}�-@}�-@}��@}�h@}`B@}?}@|��@|�@{o@z~�@z=q@y�^@y�^@y�^@yhs@y%@xQ�@w�@w��@w�@s"�@m�-@b^5@Y�7@Q��@L�j@F�R@B�H@>@6E�@1��@,9X@&5?@"~�@�/@��@��@�-@	��@V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��wA��wA��wA�A�ƨA�ĜA�ȴA���A���A���A���A���A���A�ȴA�A�ĜA��wA��FA��uA�p�A�;dA���A��wA���A�jA�&�A�+A�{A���A���A�G�A��A��-A��hA�XA��A��;A�A��-A��\A�hsA�M�A�"�A��;A���A��DA�v�A�G�A�5?A��wA�z�A�G�A��A��#A���A�t�A�v�A�x�A�~�A�|�A�l�A�v�A�ffA�n�A�z�A��A�|�A���A��
A�ZA�VA�oA��;A���A�bA���A��9A���A�O�A�/A��A���A�|�A���A�ƨA���A�jA�JA���A�33A��FA�A�A�A��!A�"�A���A��/A�ZA��`A�dZA��^A�A�E�AƨA~�9A}VA{��A{G�Az��AzbNAzAx�Aw�-Av�Av1'Au;dAt �Ar��Ao�Aj��Aj�Ai��Ah��Af{Ab�`AbjAb5?Aax�A_"�A]�A\ffA[7LAXĜAV��AVI�AV9XAV{AU��AU�-AU�AUl�AUt�AUO�AR1AOC�ANn�AM�#AMK�AJA�AHA�AF��AE�ADn�AC��ACVABI�AA�A>ȴA=�A<�A<��A;+A97LA7;dA6A�A5�mA5��A5;dA45?A3��A3�A3%A2��A2�uA2=qA1��A0ZA.��A-��A,1'A*{A'�A&ZA&�A&  A%�A%��A%�wA%��A%�A%&�A$~�A#��A r�A{Ap�A
=A�yA��A��AQ�A�A|�A~�A�7A��A��A��AVA�AA��A�/A5?A�A�mAl�A��A��A
bA	G�A��A=qAx�A�HAQ�A��A/A��AJA��A5?A�;A�hA%A �jA Z@��@��@�=q@���@��9@���@�|�@�"�@��7@�hs@�hs@�%@��D@��;@�-@�dZ@�@���@�bN@�  @��@�@�M�@�h@땁@�
=@�x�@��@�R@�+@���@�z�@��y@ᙚ@�o@�~�@�b@�-@�@���@ҸR@�  @�v�@�p�@��m@�X@��m@�5?@�@�X@��/@�z�@�|�@�n�@���@�|�@���@��7@���@��@�E�@�X@�A�@�@���@��@�`B@�Ĝ@��F@�"�@��+@��#@��D@�\)@�~�@��@�`B@��`@���@�Z@�1@���@�\)@��@��+@�5?@�x�@�%@��/@�Q�@�"�@��@�7L@��/@�r�@��@�@�^5@�x�@��j@��D@���@���@�dZ@��@�ȴ@��!@�-@��7@�/@���@�bN@�(�@�dZ@���@�@��#@�hs@�7L@��@��@�Z@�K�@���@�J@��-@�z�@��F@��@�t�@�dZ@��@��!@�5?@��7@�z�@�l�@�~�@�E�@�$�@�p�@���@�r�@�A�@���@���@�t�@�dZ@�K�@�;d@�o@�@��y@��@���@��+@�$�@��@��-@���@���@�hs@�O�@�&�@��@�V@�%@���@���@���@��D@�z�@�z�@�bN@�A�@� �@��@���@�;d@�33@�o@���@��R@��R@��\@�5?@���@��@��@��^@�X@��@���@���@�1'@�dZ@��@��H@��@���@���@�ȴ@�ȴ@�ȴ@�ȴ@���@�~�@�M�@�J@��@��-@���@�hs@�7L@�%@���@���@��@�Ĝ@��u@��@�r�@�I�@�I�@�(�@�  @l�@
=@~ȴ@~5?@}�T@}�-@}�-@}��@}�h@}`B@}?}@|��@|�@{o@z~�@z=q@y�^@y�^@y�^@yhs@y%@xQ�@w�@w��@w�@s"�@m�-@b^5@Y�7@Q��@L�j@F�R@B�H@>@6E�@1��@,9X@&5?@"~�@�/@��@��@�-@	��@V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B(�B(�B'�B&�B$�B$�B#�B"�B!�B�B�B�B�B�B�B{BuBhBVBDB1BBBBB  B  BB  B��B��B��B��B�B�B�sB�yB�`B�HB�;B�BB�TB�`B�sB�yB�B�yBɺB�PBbNB8RB'�B"�BoB��B�B��B�-B��Bz�BG�B%�B�B{BhB1B��B��B��B��B��B�VB�%By�BQ�B8RB�B
��B
��B
�B
�BB
�
B
��B
ŢB
�RB
��B
�B
jB
`BB
S�B
K�B
F�B
A�B
>wB
:^B
1'B
(�B
"�B
�B
�B
PB
  B	�fB	ǮB	ÖB	��B	�LB	��B	�oB	�VB	�DB	�B	w�B	p�B	jB	cTB	[#B	Q�B	O�B	N�B	M�B	K�B	J�B	H�B	G�B	E�B	@�B	1'B	%�B	!�B	�B	�B	VB	B	  B��B�B�B�B�yB�TB�;B�#B�B�B��B��BƨBĜBÖBB��B�qB�jB�dB�^B�^B�XB�LB�?B�'B�B��B��B��B��B��B��B��B��B��B�{B�{B�uB�bB�PB�+B}�Bv�Bt�Bs�Bs�Br�Bq�Bp�Bn�Bl�BiyBffBcTBaHB^5BZBS�BN�BI�BG�BE�BD�BC�B@�B=qB:^B6FB49B33B1'B/B.B,B+B)�B'�B%�B#�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BoBbB\B\BVBVBVBPBJBDB
=B
=B1B+B1B+B+B%BBBB  BBBBB  B  BBBBB%B+B
=B
=B
=B
=B
=BDBJBVB\BuBuB{B�B�B�B�B�B"�B#�B#�B#�B&�B'�B(�B+B.B2-B5?B6FB8RB:^B;dB<jB=qB>wB?}B@�BB�BB�BE�BG�BG�BG�BL�BO�BS�BVBW
B[#B]/B`BBdZBhsBiyBl�Bn�Bo�Bq�Br�Br�Bu�Bx�Bz�B{�B~�B~�B�B�%B�=B�=B�JB�PB�VB�PB�bB��B��B��B��B��B��B�B�B�B�B�!B�3B�LB�qBĜB��B��B��B��B��B�B�/B�;B�NB�TB�ZB�ZB�`B�fB�fB�fB�fB�fB�sB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B	  B	B	B	+B	DB	DB	PB	bB	{B	{B	�B	�B	�B	�B	�B	"�B	'�B	,B	.B	2-B	7LB	;dB	A�B	B�B	C�B	C�B	C�B	D�B	D�B	D�B	D�B	E�B	H�B	M�B	Q�B	T�B	YB	[#B	]/B	_;B	aHB	bNB	bNB	cTB	e`B	gmB	hsB	m�B	s�B	s�B	t�B	u�B	y�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�7B	�PB	�\B	�bB	�oB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	�B	ǮB	�NB	��B
B
PB
�B
�B
'�B
33B
:^B
@�B
I�B
M�B
T�B
]/B
cTB
iyB
n�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B(�B(�B'�B&�B$�B$�B#�B"�B!�B�B�B�B�B�BqBYBPBEB7B#BB�B�B�B �B��B��B �B��B��B��B��B��B�sB�\B�MB�XB�>B�'B�B�B�3B�>B�OB�WB�nB�WBɘB�)Bb&B8,B'�B"�BDB��B�lBίB�B�xBz�BG�B%�BgBOB<BB��B�TB��B�tB�UB�*B��By�BQ�B8%B�B
��B
��B
�_B
�B
��B
έB
�uB
�&B
��B
��B
jSB
`B
S�B
K�B
F�B
A_B
>LB
:5B
0�B
(�B
"�B
�B
\B
&B	��B	�@B	ǇB	�nB	�]B	�&B	��B	�JB	�/B	�B	��B	w�B	p�B	jYB	c/B	Z�B	Q�B	O�B	N�B	M�B	K�B	J�B	H�B	G�B	EB	@_B	1B	%�B	!�B	�B	oB	4B	�B��B��B�B��B�rB�WB�4B�B�B��B��B��BʢBƆB�yB�vB�nB�_B�NB�HB�CB�>B�>B�8B�,B�B�B��B��B��B��B�rB�fB�fB�cB�`B�_B�]B�YB�UB�AB�1B�B}�Bv�Bt�Bs�Bs�Br�Bq�Bp�BnyBllBiXBfEBc3Ba)B^BY�BS�BN�BI�BG�BE�BD|BCwB@bB=RB:>B6$B4B3B1B.�B-�B+�B*�B)�B'�B%�B#�B"�B!�B �B�B�ByBrBmBgB`BtBtBnBQBNBMBIBaB\BPB'B>B=BBBB3BB	B
B
B�BB�BBB�B�B�B�B��B �B �B�B �B��B��B�B�B�B�B�B
B
B
B
B
 B
B%BBB9BRBRB>BkBxB~B�B�B"�B#�B#�B#�B&�B'�B(�B*�B-�B2	B5B6 B8-B:7B;>B<FB=MB>RB?UB@^BBjBBhBE}BG�BG�BG�BL�BO�BS�BU�BV�BZ�B]B`Bd0BhLBiOBldBnqBovBq�Br�Br�Bu�Bx�Bz�B{�B~�B~�B��B��B�B�B�B�%B�-B�&B�9B�]B�iB��B��B��B��B��B��B��B��B��B�B�B�DB�nBͦBΫBϱB��B��B��B��B�B�"B�(B�*B�-B�0B�8B�5B�6B�9B�7B�BB�\B�pB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B	�B	�B	�B	B	B	!B	2B	KB	IB	]B	tB	�B	�B	�B	"�B	'�B	+�B	-�B	1�B	7B	;1B	AVB	B_B	CbB	CbB	CdB	DiB	DiB	DiB	DkB	EpB	H�B	M�B	Q�B	T�B	X�B	Z�B	\�B	_
B	aB	bB	bB	c"B	e-B	g9B	h@B	m]B	s�B	s�B	t�B	u�B	y�B	|�B	~�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�(B	�.B	�:B	�:B	�9B	�AB	�IB	�WB	�_B	�`B	�dB	��B	�yB	�B	��B
�B
B
ZB
�B
'�B
2�B
:&B
@JB
I�B
M�B
T�B
\�B
cB
iAB
n^B
s}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.47 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708222016053117082220160531170822  AO  ARCAADJP                                                                    20150115063128    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150115063128  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20150115063128  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170822  IP                  G�O�G�O�G�O�                