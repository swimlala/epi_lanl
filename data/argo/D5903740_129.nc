CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-08T09:17:17Z AOML 3.0 creation; 2016-06-01T00:08:27Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151008091717  20160531170827  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_129                   2C  D   APEX                            5374                            041511                          846 @�u@����1   @�uA�F*a@:ffffff�c�~��"�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BW��B`ffBfffBo��Bw��B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy�fD�3D�P D��fD��3D���D�C3D�l�D�� D�fD�33D���D�� D��fD�0 DږfD�ٚD�	�D�FfD�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�
=A�A'�AG�Ag�A�A�A�A�A�A�A�A�B�HB	�HB�HB�HB!�HB)�HB1�HB9�HBBG�BI�HBQ�HBYz�BbG�BhG�Bqz�Byz�B��B��B��B��B��B��B��B�#�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C xRCxRCxRCxRCxRC
xRCxRCxRCxRCxRCxRCxRCxRCxRCxRCxRC xRC"xRC$xRC&xRC(xRC*xRC,xRC.xRC0xRC2xRC4xRC6xRC8xRC:xRC<xRC>xRC@xRCBxRCDxRCFxRCHxRCJxRCLxRCNxRCPxRCRxRCTxRCVxRCXxRCZxRC\xRC^xRC`xRCbxRCdxRCfxRChxRCjxRClxRCnxRCpxRCrxRCtxRCvxRCxxRCzxRC|xRC~xRC�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�/\C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�/\C�<)C�<)C�<)C�<)C�<)C�H�C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�Dt��Dy�zD�"=D�_
D��pD��=D��D�R=D�{�D��
D�%pD�B=D���D��
D�pD�?
DڥpD��D��D�UpD�=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�?}A�?}A�7LA�+A� �A��A�oA���A��A��`A��TA��/A��
A���A���A�ƨA�Aͺ^A͸RAʹ9AͲ-AͰ!AͮAͥ�A͕�A��/Aɝ�AŰ!A��PA�S�A�A� �A���A��uA�{A��A�v�A���A��uA���A��A���A�M�A�33A���A�ĜA�/A��#A��DA�t�A��/A�"�A��;A�E�A��^A��`A���A�p�A��wA��RA���A���A�O�A�
=A��A�v�A��mA�ƨA�dZA�ZA�/A���A�^5A���A�v�A���A�;dA��A~�A|bAx�\AwS�Av�Au�FAt��Aq�wAn �Al�+Ak�Aj�yAi�-Ai+Ag��AfffAd��Ac��AcK�Ab��Ab{A`�jA_��A^��A^A]"�AYG�AXA�AW��AW+AV�AVffAU�wAU�7AUhsAUC�AT�AS��AR{AQ+AP�DAO�#AOx�AO;dAN�AN��AN��AN��ANbNAL��AK�AJ�`AJJAH�AHv�AG��AF{AEXAE�AD�/ADn�AC
=A@r�A>I�A=A<(�A;dZA:~�A9XA8ZA8(�A7��A5�A4��A4^5A3��A3S�A2�+A1�FA1C�A0��A0ĜA0��A0z�A0I�A0$�A0�A01A/�
A.��A-��A-��A-�A,�DA,JA+�A+A*�jA*E�A)VA(E�A(�A'�hA'O�A&��A%�^A$ffA"$�A!dZA r�A+AjAA�^A&�A��AE�A�A��A�HA�RA�A�A~�AS�A�`A�DAE�A�AoAI�A��A��A�AȴA��A�/Az�A�A\)A�jA��Ap�A
�DAbA�+A�mA�jA9XAK�AVA��AJA��A�A��A ��A =q@��H@��h@���@�I�@�@�{@�p�@��/@ꟾ@��@�@�|�@���@�^@���@�
=@�@���@�K�@�$�@�?}@�z�@�9X@۶F@�+@���@�r�@և+@Դ9@Ұ!@�1@Ͳ-@��;@��@ȣ�@�\)@Ĵ9@ÍP@�S�@��H@�@�v�@�Ĝ@�&�@���@��@��!@�$�@�&�@�Z@���@�^5@���@��7@��@�p�@�G�@�7L@�;d@�j@�\)@���@�hs@��u@��@�=q@��#@���@���@�x�@�G�@�/@�&�@�V@���@���@��9@�z�@�A�@�S�@�ȴ@���@�E�@���@�hs@�/@��@�Ĝ@�I�@�ƨ@�dZ@�+@�~�@�@�X@�/@��@�r�@��@�K�@�;d@�"�@�ff@�J@��7@���@��R@�v�@�n�@�M�@���@��@�j@��w@���@�J@��h@��@��9@�(�@�ƨ@���@�+@��@���@���@���@��+@�~�@�v�@�n�@�n�@�E�@�@��@���@��7@�&�@��`@��9@�z�@��u@�z�@�(�@��
@���@��@�t�@�dZ@�K�@�+@�"�@�\)@��m@� �@�Q�@�j@��@�A�@��@��^@��@��@�+@�ff@��@�@��T@���@���@�p�@�X@�G�@��@��`@��j@�r�@���@�t�@���@��@�b@���@��F@�\)@�o@�@��@��H@���@��@�C�@�K�@�33@�dZ@�l�@�|�@�;d@��R@�E�@�^5@�V@�-@�@��@���@��9@���@�Ĝ@��@�I�@��@K�@~�y@}O�@|��@|�@|�@|�/@|�j@|�@|Z@{�
@{��@{��@{t�@{S�@{�@{dZ@{t�@{��@{��@{�@{dZ@{t�@{�@{�@{�@{��@{��@{33@zM�@y��@y��@y��@yx�@yhs@yG�@y�@x��@x  @w|�@w+@u?}@m�@b~�@W|�@R��@KdZ@GK�@C�
@<�/@3C�@.��@,Z@$j@��@n�@/@1'@�@
-@	&�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�?}A�?}A�7LA�+A� �A��A�oA���A��A��`A��TA��/A��
A���A���A�ƨA�Aͺ^A͸RAʹ9AͲ-AͰ!AͮAͥ�A͕�A��/Aɝ�AŰ!A��PA�S�A�A� �A���A��uA�{A��A�v�A���A��uA���A��A���A�M�A�33A���A�ĜA�/A��#A��DA�t�A��/A�"�A��;A�E�A��^A��`A���A�p�A��wA��RA���A���A�O�A�
=A��A�v�A��mA�ƨA�dZA�ZA�/A���A�^5A���A�v�A���A�;dA��A~�A|bAx�\AwS�Av�Au�FAt��Aq�wAn �Al�+Ak�Aj�yAi�-Ai+Ag��AfffAd��Ac��AcK�Ab��Ab{A`�jA_��A^��A^A]"�AYG�AXA�AW��AW+AV�AVffAU�wAU�7AUhsAUC�AT�AS��AR{AQ+AP�DAO�#AOx�AO;dAN�AN��AN��AN��ANbNAL��AK�AJ�`AJJAH�AHv�AG��AF{AEXAE�AD�/ADn�AC
=A@r�A>I�A=A<(�A;dZA:~�A9XA8ZA8(�A7��A5�A4��A4^5A3��A3S�A2�+A1�FA1C�A0��A0ĜA0��A0z�A0I�A0$�A0�A01A/�
A.��A-��A-��A-�A,�DA,JA+�A+A*�jA*E�A)VA(E�A(�A'�hA'O�A&��A%�^A$ffA"$�A!dZA r�A+AjAA�^A&�A��AE�A�A��A�HA�RA�A�A~�AS�A�`A�DAE�A�AoAI�A��A��A�AȴA��A�/Az�A�A\)A�jA��Ap�A
�DAbA�+A�mA�jA9XAK�AVA��AJA��A�A��A ��A =q@��H@��h@���@�I�@�@�{@�p�@��/@ꟾ@��@�@�|�@���@�^@���@�
=@�@���@�K�@�$�@�?}@�z�@�9X@۶F@�+@���@�r�@և+@Դ9@Ұ!@�1@Ͳ-@��;@��@ȣ�@�\)@Ĵ9@ÍP@�S�@��H@�@�v�@�Ĝ@�&�@���@��@��!@�$�@�&�@�Z@���@�^5@���@��7@��@�p�@�G�@�7L@�;d@�j@�\)@���@�hs@��u@��@�=q@��#@���@���@�x�@�G�@�/@�&�@�V@���@���@��9@�z�@�A�@�S�@�ȴ@���@�E�@���@�hs@�/@��@�Ĝ@�I�@�ƨ@�dZ@�+@�~�@�@�X@�/@��@�r�@��@�K�@�;d@�"�@�ff@�J@��7@���@��R@�v�@�n�@�M�@���@��@�j@��w@���@�J@��h@��@��9@�(�@�ƨ@���@�+@��@���@���@���@��+@�~�@�v�@�n�@�n�@�E�@�@��@���@��7@�&�@��`@��9@�z�@��u@�z�@�(�@��
@���@��@�t�@�dZ@�K�@�+@�"�@�\)@��m@� �@�Q�@�j@��@�A�@��@��^@��@��@�+@�ff@��@�@��T@���@���@�p�@�X@�G�@��@��`@��j@�r�@���@�t�@���@��@�b@���@��F@�\)@�o@�@��@��H@���@��@�C�@�K�@�33@�dZ@�l�@�|�@�;d@��R@�E�@�^5@�V@�-@�@��@���@��9@���@�Ĝ@��@�I�@��@K�@~�y@}O�@|��@|�@|�@|�/@|�j@|�@|Z@{�
@{��@{��@{t�@{S�@{�@{dZ@{t�@{��@{��@{�@{dZ@{t�@{�@{�@{�@{��@{��@{33@zM�@y��@y��@y��@yx�@yhs@yG�@y�@x��@x  @w|�@w+@u?}@m�@b~�@W|�@R��@KdZ@GK�@C�
@<�/@3C�@.��@,Z@$j@��@n�@/@1'@�@
-@	&�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB'�B&�B&�B&�B&�B'�B'�B(�B(�B)�B+B+B,B.B/B0!B0!B1'B1'B1'B1'B1'B1'B1'B1'B&�BbB��B�B�BƨB�?B�3B�B�B��B��B�uB�bB�+B~�B{�Bn�B\)BD�B6FB+B'�B�BDB�sB��B��B��B��B�PB|�Bk�BaHBS�BH�BD�B=qB8RB2-B.B#�B{BVBJB+B
�B
�B
��B
ƨB
�jB
�3B
��B
��B
�B
cTB
VB
K�B
I�B
E�B
.B
oB
B	��B	�B	�B	�mB	�5B	��B	ƨB	ÖB	ȴB	��B	��B	ŢB	�wB	�^B	�?B	�B	��B	�uB	�\B	�PB	�DB	�1B	�B	�B	�B	�B	}�B	w�B	p�B	l�B	iyB	gmB	e`B	dZB	bNB	aHB	aHB	`BB	]/B	XB	S�B	P�B	L�B	H�B	E�B	A�B	:^B	5?B	49B	1'B	.B	%�B	�B	uB	bB	JB		7B	%B	B��B��B��B�B�B�B�B�yB�`B�NB�;B�5B�/B�/B�)B�#B�B�B�B�B��B��B��B��BȴBƨBŢBÖB�}B�jB�RB�?B�9B�-B�!B�B��B��B��B��B�hB�PB�DB�=B�1B�%B�B�B�B� B}�B|�Bz�Bx�Bt�Br�Bq�Bp�Bn�Bm�BjBhsBffBcTBaHB^5B\)B[#BYBW
BT�BR�BP�BN�BJ�BG�BD�BB�BA�B?}B>wB=qB=qB<jB<jB;dB9XB8RB6FB5?B5?B49B2-B0!B,B%�B%�B%�B$�B$�B#�B"�B!�B!�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B"�B#�B#�B#�B"�B!�B!�B&�B'�B(�B(�B(�B+B+B,B0!B2-B33B33B33B2-B1'B33B:^B<jB@�BA�BB�BG�BJ�BL�BL�BL�BM�BN�BN�BN�BO�BO�BO�BO�BP�BP�BS�BW
BW
BXBZB\)B]/B^5B^5B_;BaHBbNBcTBe`BiyBjBjBjBk�Bo�Bq�Bq�Bp�Bt�Bv�Bw�B� B�1B�=B�=B�=B�PB�VB��B��B��B��B��B�B�B�'B�3B�9B�LB�XB�^B�^B�dB�jB�qB�qB�wB�wB�}B��BBÖBŢBȴB��B��B��B��B��B��B�
B�B�B�B�B�B�#B�/B�HB�mB�B�B�B�B�B�B�yB�B�B�B�B��B��B��B��B��B	  B	B	B	B	B	B	B	+B		7B	�B	 �B	$�B	&�B	'�B	+B	/B	1'B	1'B	33B	49B	7LB	;dB	A�B	G�B	J�B	L�B	N�B	P�B	Q�B	S�B	ZB	_;B	`BB	bNB	l�B	s�B	u�B	x�B	|�B	� B	�B	�B	�7B	�DB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�3B	�9B	�?B	�LB	�XB	�^B	�jB	�}B	��B	�B
PB
uB
�B
%�B
'�B
/B
;dB
A�B
C�B
M�B
S�B
[#B
bNB
gmB
k�B
o�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B'�B&�B&�B&�B&�B'�B'�B(�B(�B)�B*�B*�B+�B-�B.�B/�B/�B1B0�B1B1B0�B1B0�B1B&�B=B��B�xB��BƁB�B�
B��B��B��B�wB�IB�8B�B~�B{�BnoB\ BDoB6B*�B'�B�BB�HBʓB��B��B�gB� B|�BkXBaBS�BH�BDlB=FB8&B2 B-�B#�BMB+BB�B
�YB
��B
зB
�zB
�=B
�B
��B
��B
��B
c(B
U�B
K�B
I�B
EvB
-�B
CB
�B	��B	�B	�UB	�EB	�B	оB	ƁB	�lB	ȍB	ΰB	ʘB	�yB	�PB	�8B	�B	��B	�kB	�MB	�4B	�)B	�B	�B	��B	��B	��B	��B	}�B	w�B	p}B	laB	iRB	gFB	e;B	d4B	b'B	a#B	a#B	`B	]B	W�B	S�B	P�B	L�B	H�B	E{B	AfB	:<B	5B	4B	1B	-�B	%�B	�B	OB	=B	$B		B	 B	�B��B��B��B�B�{B�sB�eB�VB�=B�-B�B�B�B�B�B��B��B��B��B��B��BηBͰB˥BȓBƄBŀB�tB�ZB�HB�/B�B�B�B��B��B��B��B�wB�_B�GB�.B�%B�B�B�B��B��B��B�B}�B|�Bz�Bx�Bt�Br�Bq�Bp�BnwBmqBj^BhTBfHBc4Ba(B^B\B[BX�BV�BT�BR�BP�BN�BJ�BG�BD{BBpBAiB?\B>VB=RB=RB<KB<JB;EB99B81B6$B5B5B4B2B0B+�B%�B%�B%�B$�B$�B#�B"�B!�B!�B �B �B�B�B�B�B�B}B�B�B�B�B�B�B�B�B�B�B�B �B�B"�B#�B#�B#�B"�B!�B!�B&�B'�B(�B(�B(�B*�B*�B+�B/�B2
B3B3B3B2
B1B3B:9B<EB@[BAbBBlBG�BJ�BL�BL�BL�BM�BN�BN�BN�BO�BO�BO�BO�BP�BP�BS�BV�BV�BW�BY�B\ B]B^B^B_Ba Bb&Bc.Be8BiPBjYBjYBjWBk\BovBq�Bq�Bp}Bt�Bv�Bw�B�B�B�B�B�B�$B�-B�WB�oB��B��B��B��B��B��B�B�B�B�*B�2B�1B�8B�<B�EB�CB�KB�KB�QB�\B�cB�hB�tBȈBʖB˗BͧBϱB��B��B��B��B��B��B��B��B��B�B�B�>B�RB�WB�\B�jB�lB�[B�KB�pB�bB�kB�B��B��B��B��B��B��B	 �B	 �B	�B	�B	�B	�B	�B		
B	WB	 �B	$�B	&�B	'�B	*�B	.�B	0�B	0�B	3B	4	B	7B	;1B	AVB	G}B	J�B	L�B	N�B	P�B	Q�B	S�B	Y�B	_B	`B	bB	lYB	s�B	u�B	x�B	|�B	�B	��B	��B	�B	�B	�SB	�VB	�VB	�VB	�YB	�WB	�^B	�eB	�rB	�wB	�|B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�(B	�6B	�GB	��B	�qB
B
=B
�B
%�B
'�B
.�B
;,B
AOB
C^B
M�B
S�B
Z�B
bB
g2B
kKB
oeB
qq11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.47 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708272016053117082720160531170827  AO  ARCAADJP                                                                    20151008091717    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151008091717  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151008091717  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170827  IP                  G�O�G�O�G�O�                