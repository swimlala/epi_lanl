CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-09-02T19:17:06Z AOML 3.0 creation; 2016-08-07T21:51:21Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150902191706  20160807145121  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               IA   AO  5287_9017_073                   2C  D   APEX                            6529                            072314                          846 @�lXk$�1   @�lX�5%@0��;dZ�d���$�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    IA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBq33Bw��B��B�  B�  B�  B�33B�ffB�  B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�DyY�D��D�9�D�y�D��3D�  D�6fD�i�D���D�  D�L�D���D�ɚD� D�<�D�|�D�� D�	�D�33D�l�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�
=A�A'�AG�Ag�A�A�A�A�A�A�A�A�B�HB	�HB�HB�HB!�HB)�HB1�HB9�HBA�HBI�HBQ�HBY�HBa�HBjG�Bs{Byz�B��qB��B��B��B�#�B�W
B��B��qB��B��B��B��B��B�#�B��B��B��B��BȽqB̽qB��B��B��B��B��B��B��B��B��B��B��B��C xRCxRCxRCxRCxRC
xRCxRC��CxRCxRCxRCxRCxRCxRCxRCxRC xRC"xRC$xRC&xRC(xRC*xRC,xRC.xRC0xRC2xRC4xRC6xRC8xRC:xRC<xRC>xRC@xRCB��CDxRCFxRCHxRCJxRCLxRCNxRCPxRCRxRCTxRCVxRCXxRCZxRC\xRC^xRC`xRCbxRCdxRCfxRChxRCjxRClxRCnxRCpxRCrxRCtxRCvxRCxxRCzxRC|xRC~xRC�<)C�<)C�<)C�<)C�<)C�H�C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�/\C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�/\C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt��Dyw�D��D�H�D���D��=D�
D�EpD�x�D���D�
D�[�D���D�ؤD�
D�K�Dڋ�D��
D��D�B=D�{�D��=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�~�A闍A�I�A�A��yA�9A�\A�n�A�=qA��A��A��A�ȴA�RA�A�33A���A�jA�p�A�M�A� �A���A埾A�z�A�C�A�ƨA�+A�
=A�%A��`A�+A�t�AׁA֮A�+A�ffAԕ�A�
=A��AϬA�XA�A��A�&�A�bNA�+A���A�/AŬA�K�A���A���A�+A���A��PA�M�A�VA�hsA�O�A�/A�bNA�XA���A�l�A���A�hsA�A��uA��
A�hsA��`A���A���A�?}A��HA��HA�~�A��A�S�A��#A���A��/A���A�=qA��hA�"�A���A��yA��`A��A���A�^5A���A�;dA���A�{A�`BA|9XAyAw�FAs��Ap1'An��Am�Ag��Ad=qAa��A_��A^��A]t�A[XAY�PAY�AW%AT�\ARȴAR-AP�AM��AL��AKS�AJz�AI��AHĜAG��AFA�AEl�AD�uAB��A@��A?l�A>��A=�A:�RA8n�A7S�A6v�A2ZA/�TA/&�A-A-
=A,jA+XA*ZA)��A(�A'oA& �A%��A%�A$�A#��A#�A �`A ��A �A n�Ap�Al�A�A�A�AbNA9XA��AO�A�!A�DA1'A�HA�wA�A��A�+AK�A��AG�A
=A��A�AXAr�A�PAS�AA�AK�A
AQ�A
=A  AK�A&�A�uAZA�#A=qA�^A�A�AC�A ��A �AC�A�A ��A ȴA �9A �+A (�@���@�j@�  @�dZ@��@�{@���@� �@�A�@��@��y@���@���@�M�@��^@���@�Z@�Ĝ@�M�@�l�@�E�@�I�@�1@�?}@�X@���@�w@�;d@�A�@�1'@�\)@�ȴ@�@��H@�O�@��@��`@�ƨ@�$�@�z�@� �@�  @�@��@��@�-@�=q@�^@�G�@�@�7L@�@�F@�J@�@���@㝲@�K�@���@◍@�J@�j@߮@�ȴ@�E�@ݩ�@�?}@�`B@�X@۶F@�5?@�X@��@�/@�?}@�Z@أ�@�z�@�1@�ƨ@׮@׍P@�|�@�l�@�l�@�\)@և+@��@��T@��#@��T@��T@�O�@���@��m@Ӆ@�|�@�K�@��y@�~�@�$�@��`@�j@�1'@��@�{@��@͉7@���@��
@ʧ�@�I�@���@ŉ7@őh@š�@�@�G�@�E�@�=q@��@��@�?}@� �@�l�@�"�@�
=@�@�33@��m@�b@�I�@�b@î@���@���@�~�@�5?@���@�Ĝ@�A�@���@��@�n�@��@���@�x�@�`B@�O�@���@���@�|�@��F@���@�+@�ȴ@�@��-@�hs@��u@��P@��P@��@��+@�V@�@�/@��j@�I�@��m@���@��m@�Q�@���@�C�@�o@�@�^5@�x�@���@���@�z�@�Q�@��@�ƨ@�l�@�l�@�
=@��y@��y@��y@��@��!@�=q@���@��T@�@��-@��-@���@��h@��@���@�bN@��@��F@���@�|�@��y@��!@���@�v�@�=q@��@�@�hs@�/@�O�@���@��@�  @���@�"�@��@��@�ff@��#@��-@�`B@�7L@�V@��`@��/@���@�r�@�1@��
@��@��P@�S�@���@�=q@���@�p�@�?}@���@�(�@��
@���@�S�@���@��\@��+@�5?@�@���@�X@��@���@���@�9X@�1@�\)@��H@���@�ff@�E�@�{@��@��#@���@��^@��@���@�bN@�I�@� �@��;@�r�@�V@�/@�o@z^5@r-@i��@`r�@Y�@P�`@H��@@Q�@9X@4�@-�@%��@ r�@ƨ@�P@j@ȴ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�~�A闍A�I�A�A��yA�9A�\A�n�A�=qA��A��A��A�ȴA�RA�A�33A���A�jA�p�A�M�A� �A���A埾A�z�A�C�A�ƨA�+A�
=A�%A��`A�+A�t�AׁA֮A�+A�ffAԕ�A�
=A��AϬA�XA�A��A�&�A�bNA�+A���A�/AŬA�K�A���A���A�+A���A��PA�M�A�VA�hsA�O�A�/A�bNA�XA���A�l�A���A�hsA�A��uA��
A�hsA��`A���A���A�?}A��HA��HA�~�A��A�S�A��#A���A��/A���A�=qA��hA�"�A���A��yA��`A��A���A�^5A���A�;dA���A�{A�`BA|9XAyAw�FAs��Ap1'An��Am�Ag��Ad=qAa��A_��A^��A]t�A[XAY�PAY�AW%AT�\ARȴAR-AP�AM��AL��AKS�AJz�AI��AHĜAG��AFA�AEl�AD�uAB��A@��A?l�A>��A=�A:�RA8n�A7S�A6v�A2ZA/�TA/&�A-A-
=A,jA+XA*ZA)��A(�A'oA& �A%��A%�A$�A#��A#�A �`A ��A �A n�Ap�Al�A�A�A�AbNA9XA��AO�A�!A�DA1'A�HA�wA�A��A�+AK�A��AG�A
=A��A�AXAr�A�PAS�AA�AK�A
AQ�A
=A  AK�A&�A�uAZA�#A=qA�^A�A�AC�A ��A �AC�A�A ��A ȴA �9A �+A (�@���@�j@�  @�dZ@��@�{@���@� �@�A�@��@��y@���@���@�M�@��^@���@�Z@�Ĝ@�M�@�l�@�E�@�I�@�1@�?}@�X@���@�w@�;d@�A�@�1'@�\)@�ȴ@�@��H@�O�@��@��`@�ƨ@�$�@�z�@� �@�  @�@��@��@�-@�=q@�^@�G�@�@�7L@�@�F@�J@�@���@㝲@�K�@���@◍@�J@�j@߮@�ȴ@�E�@ݩ�@�?}@�`B@�X@۶F@�5?@�X@��@�/@�?}@�Z@أ�@�z�@�1@�ƨ@׮@׍P@�|�@�l�@�l�@�\)@և+@��@��T@��#@��T@��T@�O�@���@��m@Ӆ@�|�@�K�@��y@�~�@�$�@��`@�j@�1'@��@�{@��@͉7@���@��
@ʧ�@�I�@���@ŉ7@őh@š�@�@�G�@�E�@�=q@��@��@�?}@� �@�l�@�"�@�
=@�@�33@��m@�b@�I�@�b@î@���@���@�~�@�5?@���@�Ĝ@�A�@���@��@�n�@��@���@�x�@�`B@�O�@���@���@�|�@��F@���@�+@�ȴ@�@��-@�hs@��u@��P@��P@��@��+@�V@�@�/@��j@�I�@��m@���@��m@�Q�@���@�C�@�o@�@�^5@�x�@���@���@�z�@�Q�@��@�ƨ@�l�@�l�@�
=@��y@��y@��y@��@��!@�=q@���@��T@�@��-@��-@���@��h@��@���@�bN@��@��F@���@�|�@��y@��!@���@�v�@�=q@��@�@�hs@�/@�O�@���@��@�  @���@�"�@��@��@�ff@��#@��-@�`B@�7L@�V@��`@��/@���@�r�@�1@��
@��@��P@�S�@���@�=q@���@�p�@�?}@���@�(�@��
@���@�S�@���@��\@��+@�5?@�@���@�X@��@���@���@�9X@�1@�\)@��H@���@�ff@�E�@�{@��@��#@���@��^@��@���@�bN@�I�@� �G�O�@�r�@�V@�/@�o@z^5@r-@i��@`r�@Y�@P�`@H��@@Q�@9X@4�@-�@%��@ r�@ƨ@�P@j@ȴ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	�)B	�;B	�TB	�B
VB
uB
oB
�B
�B
'�B
/B
8RB
L�B
XB
[#B
\)B
_;B
bNB
k�B
y�B
~�B
�B
�B
�1B
�+B
�+B
�=B
��B
�B
�B
�qB
�B
�B1B�B)�B_;B��B�qB�BB��BoBC�BbNBl�B|�B�=B�9B��B�yBhB�B�B#�B%�B�B�/B�wB�XB�'BB��B�XB��B�oBjBx�Bw�BffB`BBQ�B6FB�BB�BuBB�NB�wB��Bq�Bk�B_;B[#BN�B=qB'�B
=B
��B
�B
�hB
XB
6FB
'�B
�B	��B	�B	�TB	ȴB	ŢB	B	�LB	��B	�7B	|�B	v�B	r�B	l�B	bNB	YB	S�B	H�B	?}B	8RB	49B	-B	!�B	�B	{B	bB	�B	�B	�B	uB	PB	%B	  B��B��B��B�B�fB�;B�B��B��BǮBƨB��B��B��B�
B�
B��B�B�)B�#B�;B�HB�HB�TB�HB�fB�mB�sB�sB�B�fB�mB�B�B��B��B	  B	B	DB	PB	PB	PB	hB	{B	�B	{B	uB	uB	�B	�B	�B	�B	�B	�B	 �B	'�B	0!B	/B	&�B	�B		7B	B��B��B��B	1B	JB	{B	�B	oB	�B	�B	 �B	'�B	2-B	5?B	7LB	7LB	<jB	=qB	:^B	-B	)�B	)�B	(�B	.B	M�B	[#B	_;B	aHB	ffB	v�B	p�B	hsB	n�B	s�B	y�B	x�B	|�B	�DB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�9B	��B	�}B	B	ƨB	��B	�dB	�LB	�FB	�FB	�?B	�9B	�9B	�XB	�dB	�jB	B	��B	��B	��B	��B	��B	ƨB	ŢB	ŢB	ɺB	ȴB	ǮB	ǮB	ĜB	ÖB	ĜB	ĜB	ĜB	ŢB	��B	��B	��B	ȴB	ǮB	ȴB	��B	��B	��B	�B	�)B	�)B	�)B	�)B	�)B	�/B	�5B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�NB	�NB	�HB	�;B	�HB	�HB	�HB	�NB	�NB	�HB	�BB	�BB	�HB	�BB	�;B	�5B	�5B	�)B	�B	��B	ȴB	��B	B	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	�
B	�
B	�B	�
B	�B	�B	�)B	�HB	�NB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�yB	�yB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
	7B

=B
DB
JB
JB
PB
\B
bB
bB
bB
bB
bB
bB
hB
bB
hB
oB
oB
oB
oB
oB
oB
oB
oB
oB
hB
hB
bB
hB
hB
uB
uB
hB
�B
�B
$�B
0!B
49B
<jB
B�B
F�B
N�B
S�B
YB
]/B
cTB
gmB
k�B
p�B
t�B
w�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	�B	�B	�B	�#B	�:B	�B
>B
ZB
SB
nB
�B
'�B
/ B
86B
L�B
W�B
[B
\B
_ B
b2B
kjB
y�B
~�B
��B
�B
�B
�B
�
B
�B
��B
��B
��B
�PB
��B
�|BB�B)�B_B��B�JB�B��BHBCpBb(BleB|�B�B�B��B�TBBB|BhB#�B%�BlB�B�PB�1B��B�fB�]B�/B��B�FBjWBx�Bw�Bf<B`BQ�B6BkB�B]BIB �B�"B�IB��Bq}BkYB_BZ�BN�B=EB'�B
B
ͨB
��B
�=B
W�B
6B
'�B
�B	��B	�tB	�,B	ȎB	�B	�iB	�(B	�xB	�B	|�B	v�B	r�B	ldB	b)B	X�B	S�B	H�B	?WB	8.B	4B	,�B	!�B	�B	ZB	@B	hB	�B	kB	PB	.B	 B��B��B��B��B��B�DB�B��B��BʟBǉBƄBʟBϽBϼB��B��B��B��B�B��B�B�#B�$B�2B� B�@B�IB�MB�NB�XB�AB�GB�qB�}B��B��B��B	�B	B	(B	*B	'B	@B	SB	WB	RB	KB	MB	\B	cB	sB	|B	�B	�B	 �B	'�B	/�B	.�B	&�B	�B		B	�B��B��B��B	B	 B	RB	WB	EB	\B	�B	 �B	'�B	2 B	5B	7B	7 B	<;B	=FB	:2B	,�B	)�B	)�B	(�B	-�B	M�B	Z�B	_B	aB	f8B	v�B	ptB	hFB	nhB	s�B	y�B	x�B	|�B	�B	�QB	�]B	�bB	�eB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�WB	�MB	�\B	�wB	�XB	�2B	�B	�B	�B	�B	�B	�B	�%B	�2B	�9B	�^B	̛B	ϪB	ϬB	͠B	ʎB	�uB	�pB	�oB	ɆB	ȂB	�{B	�{B	�iB	�cB	�kB	�iB	�jB	�qB	ʎB	ΨB	˕B	ȂB	�|B	ȂB	̙B	ϭB	ҾB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	ѸB	ȀB	�VB	�[B	�cB	�kB	�uB	ȀB	ѸB	ҾB	ҿB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�&B	�-B	�3B	�8B	�@B	�?B	�DB	�PB	�IB	�IB	�IB	�DB	�EB	�@B	�?B	�>B	�?B	�?B	�>B	�=B	�CB	�\B	�cB	�cB	�eB	�]B	�cB	�ZB	�QB	�VB	�ZB	�PB	�QB	�IB	�FB	�EB	�CB	�IB	�RB	�VB	�]B	�jB	�qB	�eB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
	B

B
B
B
B
B
$B
,B
-B
)B
.B
+B
,B
1B
-B
0B
6B
8B
6B
7B
7B
8B
6B
6B
7B
2B
0B
,B
1B
2G�O�B
;B
1B
tB
zB
$�B
/�B
4B
<3B
BVB
FoB
N�B
S�B
X�B
\�B
cB
g1B
kNB
phB
t�B
w�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.47 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451212016080714512120160807145121  AO  ARCAADJP                                                                    20150902191706    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150902191706  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150902191706  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145121  IP                  G�O�G�O�G�O�                