CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-04-02T19:17:05Z AOML 3.0 creation; 2016-08-07T21:51:16Z UW 3.1 conversion     
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
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150402191705  20160807145116  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ,A   AO  5287_9017_044                   2C  D   APEX                            6529                            072314                          846 @�F'��1   @�F"��@1�V�u�dēt�j1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ,A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D�	�D�@ D�s3D��fD� D�6fD�� D��3D���D�I�D���D�y�D� D�9�Dڙ�D��3D�� D�FfD�y�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=p@�
=A�A'�AG�Ag�A�A�A�A�A�A�A�A�B�HB	�HB�HB�HB!�HB)�HB1�HB9�HBA�HBI�HBQ�HBY�HBa�HBi�HBq�HBy�HB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�#�B��B��B��B��B��B��B��B��B��B��B��B��B��C xRCxRCxRCxRCxRC
xRCxRCxRCxRCxRCxRCxRC��C��CxRCxRC xRC"xRC$xRC&xRC(xRC*xRC,xRC.xRC0xRC2xRC4xRC6xRC8xRC:xRC<xRC>xRC@xRCBxRCDxRCF��CHxRCJxRCLxRCNxRCPxRCRxRCTxRCVxRCXxRCZxRC\xRC^xRC`xRCbxRCdxRCfxRChxRCjxRClxRCnxRCpxRCrxRCtxRCvxRCxxRCzxRC|xRC~xRC�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�/\C�<)C�<)C�<)C�H�C�H�C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�GDy��D��D�O
D��=D��pD�
D�EpD��
D��=D��D�X�D���Dǈ�D�
D�H�Dڨ�D��=D��
D�UpD�D��p11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A���A���A��
A��
A���A���A̶FȀ\A�r�A�|�A�~�Ȧ+A̅A�v�A�VA�E�A�9XA�-A�bA�  A�A�A��A���A�  A�
=A�  AˍPA��A� �A��A�VA�JA�JA�A�O�A�?}A�&�A���A�A�?}A�1'AƟ�A�ZA�jA�ZA�5?A�I�A��A���AżjA�`BA�C�A���Aě�A�;dA�  A��A��`A��A���A�dZA�{A¬A��A��A���A��7A�p�A�"�A�1'A���A���A�&�A��A�XA�z�A�9XA��PA�M�A���A���A�bNA���A�A�bNA�
=A�33A���A��A�v�A��!A��mA�Q�A���A��A���A��A�oA��`A��RA�~�A�bA�A��DA��^A�-A��A�?}A��+A���AS�A|�HAz$�Au�PAm+Aip�Agp�AeC�Ac��Abz�A`v�A]�7AY�
AX9XAVffAT�AQ�AQ%AOt�AI��AES�AC�AA�AA�A?�FA>�A;+A9�
A9\)A8v�A7�FA6��A3�hA1"�A0I�A/;dA.{A,v�A+��A*bA(�A((�A'/A$��A#��A#XA"��A"�+A!�A!�hA �A ��A 1A|�A�A�HA�TA%AM�AoA�AS�A�yA��Av�A�An�A��A�+A%A��A�9AȴA(�A?}A �AA�/A=qA��A�A��A  A�A��A$�A=qAK�A �\A A�@��m@�S�@���@���@�J@��`@��@�=q@�S�@��y@�x�@���@�ƨ@��@��h@��@�P@�dZ@��@�@�hs@�/@�u@띲@�dZ@땁@��@��@�I�@��m@���@�S�@�n�@�@噚@��@��/@�D@�  @�+@�J@�@�j@��m@�o@ݲ-@��`@���@��@��@ӍP@�33@��@Ѳ-@�x�@щ7@�%@д9@�(�@ύP@Ϯ@Ϯ@�"�@Η�@�=q@�V@͙�@�G�@��`@�z�@�Q�@��
@˾w@�t�@��H@��@���@�hs@�7L@�G�@�G�@�`B@�&�@�&�@�?}@�/@���@���@Ȭ@�j@�(�@��@��m@��m@���@�S�@�E�@���@��#@�@Ł@�`B@�&�@��@��@�V@���@ēu@���@�G�@�%@ģ�@ģ�@��`@�Ĝ@��@�+@�E�@�-@�X@�l�@���@�?}@��9@���@���@���@�hs@�G�@��@��`@�9X@�ƨ@�  @�j@�  @��@�33@�E�@�{@��@�@��@�Ĝ@��D@���@��D@��u@��@��9@�bN@��@�  @��
@���@���@�~�@�=q@�@���@���@�G�@��D@�A�@� �@��;@���@��P@�
=@���@���@�M�@��@�@���@���@�p�@�&�@�r�@�b@��
@���@�l�@�\)@�K�@�@�M�@���@��T@���@�G�@���@��u@�1'@��@�+@���@���@�E�@�J@���@��h@�`B@��`@��u@���@��P@�
=@���@�M�@�@�`B@��@���@��`@�r�@�A�@�(�@�b@��
@���@�K�@��@�~�@���@�p�@�X@�?}@��/@��@��@�A�@��
@���@�dZ@�"�@��R@�{@�X@���@��D@�1'@��@�K�@���@��!@�v�@�@��@��-@�hs@���@�r�@��@���@�ƨ@��@���@���@�l�@�+@���@�^5@�M�@�@�hs@�7L@�%@��u@�A�@���@��F@��F@��@���@��@�|�@�K�@��y@��h@���@�bN@��u@x �@m�-@d�/@]O�@X  @OK�@Hb@>ff@5�h@-��@)�7@#�
@��@?}@��@%@z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A���A���A���A���A���A���A���A���A���A���A��
A��
A���A���A̶FȀ\A�r�A�|�A�~�Ȧ+A̅A�v�A�VA�E�A�9XA�-A�bA�  A�A�A��A���A�  A�
=A�  AˍPA��A� �A��A�VA�JA�JA�A�O�A�?}A�&�A���A�A�?}A�1'AƟ�A�ZA�jA�ZA�5?A�I�A��A���AżjA�`BA�C�A���Aě�A�;dA�  A��A��`A��A���A�dZA�{A¬A��A��A���A��7A�p�A�"�A�1'A���A���A�&�A��A�XA�z�A�9XA��PA�M�A���A���A�bNA���A�A�bNA�
=A�33A���A��A�v�A��!A��mA�Q�A���A��A���A��A�oA��`A��RA�~�A�bA�A��DA��^A�-A��A�?}A��+A���AS�A|�HAz$�Au�PAm+Aip�Agp�AeC�Ac��Abz�A`v�A]�7AY�
AX9XAVffAT�AQ�AQ%AOt�AI��AES�AC�AA�AA�A?�FA>�A;+A9�
A9\)A8v�A7�FA6��A3�hA1"�A0I�A/;dA.{A,v�A+��A*bA(�A((�A'/A$��A#��A#XA"��A"�+A!�A!�hA �A ��A 1A|�A�A�HA�TA%AM�AoA�AS�A�yA��Av�A�An�A��A�+A%A��A�9AȴA(�A?}A �AA�/A=qA��A�A��A  A�A��A$�A=qAK�A �\A A�@��m@�S�@���@���@�J@��`@��@�=q@�S�@��y@�x�@���@�ƨ@��@��h@��@�P@�dZ@��@�@�hs@�/@�u@띲@�dZ@땁@��@��@�I�@��m@���@�S�@�n�@�@噚@��@��/@�D@�  @�+@�J@�@�j@��m@�o@ݲ-@��`@���@��@��@ӍP@�33@��@Ѳ-@�x�@щ7@�%@д9@�(�@ύP@Ϯ@Ϯ@�"�@Η�@�=q@�V@͙�@�G�@��`@�z�@�Q�@��
@˾w@�t�@��H@��@���@�hs@�7L@�G�@�G�@�`B@�&�@�&�@�?}@�/@���@���@Ȭ@�j@�(�@��@��m@��m@���@�S�@�E�@���@��#@�@Ł@�`B@�&�@��@��@�V@���@ēu@���@�G�@�%@ģ�@ģ�@��`@�Ĝ@��@�+@�E�@�-@�X@�l�@���@�?}@��9@���@���@���@�hs@�G�@��@��`@�9X@�ƨ@�  @�j@�  @��@�33@�E�@�{@��@�@��@�Ĝ@��D@���@��D@��u@��@��9@�bN@��@�  @��
@���@���@�~�@�=q@�@���@���@�G�@��D@�A�@� �@��;@���@��P@�
=@���@���@�M�@��@�@���@���@�p�@�&�@�r�@�b@��
@���@�l�@�\)@�K�@�@�M�@���@��T@���@�G�@���@��u@�1'@��@�+@���@���@�E�@�J@���@��h@�`B@��`@��u@���@��P@�
=@���@�M�@�@�`B@��@���@��`@�r�@�A�@�(�@�b@��
@���@�K�@��@�~�@���@�p�@�X@�?}@��/@��@��@�A�@��
@���@�dZ@�"�@��R@�{@�X@���@��D@�1'@��@�K�@���@��!@�v�@�@��@��-@�hs@���@�r�@��@���@�ƨ@��@���@���@�l�@�+@���@�^5@�M�@�@�hs@�7L@�%@��u@�A�@���@��F@��F@��@���@��@�|�@�K�G�O�@��h@���@�bN@��u@x �@m�-@d�/@]O�@X  @OK�@Hb@>ff@5�h@-��@)�7@#�
@��@?}@��@%@z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
y�B
�bB
�B
��B
��B
�B
�
B
�)B
�)B
�5B
�fB
�B
��B
��B+B{B!�B'�B1'B9XB?}BC�BYB�B��B�?B��B�
B�#B�TB+B-B0!B49B6FB;dB9XB<jB;dBD�BL�BVB|�B�B�wBŢBĜBŢB��B�;B�B�B�B��B��B��B��BoB�B'�B!�BDB��B��B��B��Bt�Bq�Bs�BdZBA�B;dB2-BE�B`BBiyB_;BVBD�B?}B;dB:^B7LB49B/B'�B�B	7B�TB�jB��B�DBo�BR�B>wB&�BVB
�B
�B
ɺB
�B
�bB
gmB
K�B
A�B
6FB
#�B
PB	��B	�B	��B	�JB	� B	r�B	iyB	`BB	S�B	C�B	33B	(�B	�B	�B	1B	B��B�B�BB�5B�B�B��B��B��B��B�B�)B�B�B�B��B��B��B��BȴBǮBȴB��B��B��B��B��B��B��B��B�
B�
B�B�B�/B�/B�5B�/B�HB�NB�TB�`B�fB�sB�yB�sB�sB�fB�mB�B�B�;B�#B�B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B	B	1B	B	%B	B	%B	uB	�B	hB	PB	
=B		7B	1B	  B��B��B��B��B��B��B	B	B	B	
=B	oB	oB	�B	�B	"�B	%�B	(�B	+B	/B	8RB	:^B	<jB	=qB	?}B	D�B	J�B	M�B	O�B	P�B	T�B	T�B	W
B	Q�B	D�B	D�B	H�B	J�B	G�B	I�B	M�B	T�B	W
B	YB	[#B	`BB	cTB	cTB	dZB	gmB	m�B	n�B	r�B	u�B	v�B	v�B	w�B	y�B	x�B	x�B	|�B	}�B	}�B	� B	�B	�%B	�+B	�1B	�DB	�PB	�VB	�VB	�VB	�VB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�3B	�?B	�9B	�3B	�-B	�LB	�RB	�FB	�?B	�9B	�3B	�3B	�-B	�!B	�-B	�3B	�-B	�3B	�FB	�^B	�jB	��B	B	ÖB	ƨB	ŢB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�HB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B

=B
bB
�B
�B
$�B
/B
33B
<jB
A�B
E�B
I�B
Q�B
ZB
`BB
dZB
gmB
jB
m�B
q�B
t�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
y�B
�BB
��B
ϸB
��B
��B
��B
�B
�B
�B
�@B
�mB
��B
��BBYB!�B'�B1B94B?WBCtBX�B��B��B�B̦B��B��B�0BB,�B/�B4B6 B;AB90B<BB;?BDuBL�BU�B|�B��B�SB�xB�uB�yBͭB�B�qB�B�B��B��B��B��BNB�B'�B!�B"B��B��B�ZB�|Bt�Bq�Bs�Bd/BAaB;<B2BEuB`BiMB_BU�BDpB?OB;:B:4B7!B4B.�B'�B�B	
B�)B�?B��B�BopBR�B>MB&�B*B
�B
��B
ɏB
��B
�8B
gCB
K�B
A`B
6B
#�B
'B	��B	��B	��B	�$B	�B	r�B	iVB	`B	S�B	CnB	3B	(�B	�B	dB	B	�B��B�]B�B�B��B��B��BͲBͱBϽB��B�B��B��B��B��B��B̪BʝBȑBǈBȑBϻBͮBˢB��B��B��B��B��B��B��B��B��B�B�	B�B�
B�!B�(B�/B�9B�BB�NB�TB�KB�NB�@B�EB�aB�`B�B��B��B��B��B϶B��BΰB̦B��B�YB�B��B��B��B��B��B��B��B��B��B��B��B	�B	B	�B	�B	�B	�B	HB	^B	>B	'B	
B		B	B��B��B��B��B��B��B��B	 �B	 �B	�B	
B	DB	CB	\B	{B	"�B	%�B	(�B	*�B	.�B	8$B	:2B	<<B	=BB	?PB	DmB	J�B	M�B	O�B	P�B	T�B	T�B	V�B	Q�B	DqB	DmB	H�B	J�B	GB	I�B	M�B	T�B	V�B	X�B	Z�B	`B	c$B	c%B	d,B	g<B	mcB	ngB	r�B	u�B	v�B	v�B	w�B	y�B	x�B	x�B	|�B	}�B	}�B	�B	��B	��B	��B	��B	�B	� B	�#B	�$B	�%B	�$B	�+B	�OB	�aB	�`B	�aB	�`B	�[B	�VB	�[B	�aB	�hB	�rB	�sB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	��B	�B	�B	�B	�
B	�B	� B	�B	��B	��B	��B	�B	��B	�B	�B	�/B	�5B	�OB	�YB	�cB	�uB	�nB	�oB	�zB	ɆB	ʏB	ʌB	͞B	ΤB	ѹB	ҿB	ҾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�$B	�%B	�&B	�)B	�7B	�?B	�CB	�DB	�HB	�UB	�VB	�_B	�]B	�aB	�cB	�aB	�aB	�hB	�iB	�jB	�hB	�nB	�nB	�tB	�mB	�uB	�yB	�wB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B

B
+B
nB
�B
$�B
.�B
2�B
<3B
AQB
EjB
I�B
Q�B
Y�B
`	B
d"B
g4B
jEB
mXB
qvB
t�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.47 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451162016080714511620160807145116  AO  ARCAADJP                                                                    20150402191705    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150402191705  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150402191705  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145116  IP                  G�O�G�O�G�O�                