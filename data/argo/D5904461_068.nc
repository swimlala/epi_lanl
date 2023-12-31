CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-08-26T19:18:06Z AOML 3.0 creation; 2016-08-07T21:36:38Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150826191806  20160807143638  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               DA   AO  5286_8897_068                   2C  D   APEX                            6531                            072314                          846 @�j�*��	1   @�j���ґ@4�����cJ�G�{1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    DA   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BG��BPffBW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc�fDd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�DyL�D�fD�9�D�� D�ɚD� D�P D���D��fD�  D�P D�vfD�� D�	�D�,�Dڙ�D�� D��D�0 D� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��G@�z�A=qA"=qAB=qAb=qA��A��A��A��A��A��A��A��B �\B�\B�\B��B �\B(�\B0�\B8�\B@�\BH(�BP��BX(�B`�\Bh�\Bp�\Bx�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�z�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C #�C#�C#�C#�C#�C
#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C #�C"#�C$#�C&#�C(#�C*#�C,#�C.#�C0#�C2#�C4#�C6#�C8#�C:#�C<#�C>#�C@#�CB#�CD#�CF#�CH#�CJ#�CL#�CN#�CP#�CR#�CT#�CV#�CX#�CZ#�C\#�C^#�C`#�Cb#�Cd#�Cf#�Ch#�Cj#�Cl#�Cn#�Cp#�Cr#�Ct#�Cv#�Cx#�Cz#�C|#�C~#�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc�\Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��DyU�D�
�D�>D��{D��D�{D�T{D��D���D�{D�T{D�z�D��{D�D�1HDڞD��{D�HD�4{D�{D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�l�A�p�A�bNA�z�A�G�A�M�A�O�A�S�A�ZA�`BA�XA�M�A�M�A�^5A�r�A�r�A�K�A�?}A�9XA�A�A�33A��A���A�ȴA�l�A��A�"�A��`A̼jA�
=A�O�AʶFA��Aǥ�AƸRA��A�%A��A�n�A�`BA���A�K�A�G�A�ƨA�S�A��
A�^5A��\A�^5A�33A�\)A��PA�&�A���A���A���A��TA��A���A��+A�~�A�n�A�A�A���A��A��A�A�r�A��wA��`A�bNA��9A��A�9XA�ȴA��A���A�{A�&�A��!A���A��\A�+A�?}A�l�A� �A�%A�t�A�%A�/A�dZA��9A��hA�S�A�bNA��#A�hsA�(�A���A�A�A�bA��+A�/A��A���A��#A�hsA~��A|{Au��Ar^5ApĜAnJAk�Ai&�Ag��Ae�mAb�A_O�A]O�A\jA\�AZ�AV�AUG�AR1APAN�AMG�AKx�AH~�AGG�ADbNAA7LA?�A>VA=hsA;�A9A9?}A8��A6�DA4^5A3��A2E�A0E�A-��A+hsA)��A(�A(~�A'��A'VA%l�A#�A ��AC�A�AbA9XA�A�HA��A��A�Az�A;dAC�A33A7LA�A�HA�yAhsA�#A�7A+A�+A�AA�A�AZA-AJA��A��AO�A��A
=A33A��A��A1'Ar�A�A�/A�A��A$�A�mA��A��A��A�RA�RA+A�TA��A�PA�AoA�A�A"�A
�A~�Al�A�A5?A�HAx�A {@��\@��D@�@�E�@��@�5?@�
=@�5?@��@��m@�K�@�b@�1@�dZ@���@���@��@�@�V@�^5@�R@��@�`B@��@���@�Q�@���@�
=@��T@�$�@���@�I�@��@�@�t�@��@�  @�C�@�+@���@�"�@�;d@��@睲@��@��@�?}@�Q�@���@��T@�I�@ڸR@�O�@ו�@�{@թ�@���@�$�@�33@���@�C�@�Ĝ@��H@׶F@��@�/@˾w@���@ȃ@�9X@�%@ɲ-@���@�Q�@�  @�@��@��@��/@�I�@̴9@́@���@��@ȼj@�5?@�l�@�v�@��@��@��@Ƈ+@Ǯ@�`B@�/@�7L@Ɂ@�`B@ǍP@�M�@�S�@�p�@���@��`@�z�@�j@��D@��;@�ƨ@��@�z�@�p�@��@��9@�ƨ@�+@�ȴ@�-@�@���@��m@��
@�$�@���@���@�Ĝ@���@���@�x�@��j@��u@�j@�b@��@�;d@�\)@�\)@�33@�
=@��y@��R@�ff@���@���@�Q�@��@�j@�l�@�@�O�@���@�1@��@��@��w@�j@���@��T@�V@��j@��@�9X@���@�\)@��y@�=q@��^@�X@�?}@��@�@��!@�=q@��#@�`B@��@���@��j@���@���@�A�@��;@��P@�dZ@�
=@��\@��^@��`@��F@�bN@��9@���@��j@�r�@���@��@�"�@�
=@��@��@�^5@��@���@�Z@�Q�@�A�@�(�@�  @���@��w@�|�@��y@���@�^5@��@��^@�z�@��@���@��w@��F@���@�@��y@�ȴ@�n�@�J@��h@�G�@�/@�/@�&�@�V@���@�Ĝ@��D@�Z@�b@�ƨ@���@���@��P@���@��P@�K�@��@��@�;d@�C�@�;d@��@���@�E�@�5?@�J@���@��T@��7@�&�@���@��D@��@�j@��@�O�@���@�^5@x�9@rM�@i�^@`Ĝ@XQ�@Q&�@I&�@Ct�@<�@5�-@/�@(��@$��@ bN@^5@@r�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�l�A�p�A�bNA�z�A�G�A�M�A�O�A�S�A�ZA�`BA�XA�M�A�M�A�^5A�r�A�r�A�K�A�?}A�9XA�A�A�33A��A���A�ȴA�l�A��A�"�A��`A̼jA�
=A�O�AʶFA��Aǥ�AƸRA��A�%A��A�n�A�`BA���A�K�A�G�A�ƨA�S�A��
A�^5A��\A�^5A�33A�\)A��PA�&�A���A���A���A��TA��A���A��+A�~�A�n�A�A�A���A��A��A�A�r�A��wA��`A�bNA��9A��A�9XA�ȴA��A���A�{A�&�A��!A���A��\A�+A�?}A�l�A� �A�%A�t�A�%A�/A�dZA��9A��hA�S�A�bNA��#A�hsA�(�A���A�A�A�bA��+A�/A��A���A��#A�hsA~��A|{Au��Ar^5ApĜAnJAk�Ai&�Ag��Ae�mAb�A_O�A]O�A\jA\�AZ�AV�AUG�AR1APAN�AMG�AKx�AH~�AGG�ADbNAA7LA?�A>VA=hsA;�A9A9?}A8��A6�DA4^5A3��A2E�A0E�A-��A+hsA)��A(�A(~�A'��A'VA%l�A#�A ��AC�A�AbA9XA�A�HA��A��A�Az�A;dAC�A33A7LA�A�HA�yAhsA�#A�7A+A�+A�AA�A�AZA-AJA��A��AO�A��A
=A33A��A��A1'Ar�A�A�/A�A��A$�A�mA��A��A��A�RA�RA+A�TA��A�PA�AoA�A�A"�A
�A~�Al�A�A5?A�HAx�A {@��\@��D@�@�E�@��@�5?@�
=@�5?@��@��m@�K�@�b@�1@�dZ@���@���@��@�@�V@�^5@�R@��@�`B@��@���@�Q�@���@�
=@��T@�$�@���@�I�@��@�@�t�@��@�  @�C�@�+@���@�"�@�;d@��@睲@��@��@�?}@�Q�@���@��T@�I�@ڸR@�O�@ו�@�{@թ�@���@�$�@�33@���@�C�@�Ĝ@��H@׶F@��@�/@˾w@���@ȃ@�9X@�%@ɲ-@���@�Q�@�  @�@��@��@��/@�I�@̴9@́@���@��@ȼj@�5?@�l�@�v�@��@��@��@Ƈ+@Ǯ@�`B@�/@�7L@Ɂ@�`B@ǍP@�M�@�S�@�p�@���@��`@�z�@�j@��D@��;@�ƨ@��@�z�@�p�@��@��9@�ƨ@�+@�ȴ@�-@�@���@��m@��
@�$�@���@���@�Ĝ@���@���@�x�@��j@��u@�j@�b@��@�;d@�\)@�\)@�33@�
=@��y@��R@�ff@���@���@�Q�@��@�j@�l�@�@�O�@���@�1@��@��@��w@�j@���@��T@�V@��j@��@�9X@���@�\)@��y@�=q@��^@�X@�?}@��@�@��!@�=q@��#@�`B@��@���@��j@���@���@�A�@��;@��P@�dZ@�
=@��\@��^@��`@��F@�bN@��9@���@��j@�r�@���@��@�"�@�
=@��@��@�^5@��@���@�Z@�Q�@�A�@�(�@�  @���@��w@�|�@��y@���@�^5@��@��^@�z�@��@���@��w@��F@���@�@��y@�ȴ@�n�@�J@��h@�G�@�/@�/@�&�@�V@���@�Ĝ@��D@�Z@�b@�ƨ@���@���@��P@���@��P@�K�@��@��@�;d@�C�@�;d@��@���@�E�@�5?@�J@���@��T@��7@�&�@���@��D@��@�jG�O�@�O�@���@�^5@x�9@rM�@i�^@`Ĝ@XQ�@Q&�@I&�@Ct�@<�@5�-@/�@(��@$��@ bN@^5@@r�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�BB
�BB
�5B
��B
ȴB
��B
��B
��B
�B
�#B
�#B
�B
�;B
�B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�ZB%B6FBC�B:^B6FBZB��B��B��B�B�B33BG�BG�BH�BL�B`BBr�B�B�B� B�VB�B�B�B1B�B�B�B�B�B�B�B�BoBuBJB��B�B�BBhB�B1B��B�B�ZBƨB��BiyBD�B+B��B�
B�3B��Bu�Bp�Bn�B��B��B�3B�oBx�BgmB]/BK�BE�BK�BR�BE�B �B$�B�BbBB
��B
�)B
��B
{�B
s�B
jB
\)B
F�B
hB	��B	ȴB	�B	��B	�DB	w�B	aHB	P�B	A�B	33B	!�B	�B	�B	uB		7B��B�sB�B��BɺBB�XB�9B�3B�B��B��B�{B�oB��B�B�B�B��B��B��B��B�B�3B�B��B��B��B��B��B�B�9B�'B��B��B�JB|�Bz�Bv�Bt�B�B�hB��B�^B�qBBɺBɺB��B�)B��B		7B		7B	+B	B��B��B��B��B�B��B��B	B	DB	bB	\B	�B	#�B	6FB	=qB	L�B	ZB	^5B	bNB	dZB	`BB	\)B	cTB	_;B	W
B	R�B	R�B	S�B	_;B	l�B	q�B	n�B	l�B	l�B	s�B	v�B	u�B	p�B	gmB	aHB	_;B	[#B	S�B	F�B	1'B	0!B	+B	#�B	5?B	8RB	?}B	F�B	E�B	E�B	C�B	D�B	L�B	N�B	N�B	R�B	W
B	ZB	O�B	J�B	L�B	W
B	cTB	q�B	v�B	v�B	z�B	}�B	� B	~�B	|�B	z�B	{�B	{�B	|�B	|�B	{�B	y�B	y�B	y�B	y�B	�7B	�JB	�oB	�oB	�PB	�B	~�B	� B	�B	{�B	x�B	w�B	r�B	n�B	k�B	jB	l�B	p�B	z�B	�B	�B	{�B	�=B	�bB	�B	o�B	XB	S�B	P�B	P�B	W
B	^5B	hsB	z�B	{�B	�PB	��B	��B	�oB	�uB	��B	��B	��B	��B	�uB	��B	��B	��B	��B	��B	��B	��B	�B	�^B	�^B	�dB	�jB	�dB	�FB	�'B	��B	��B	��B	�B	�B	�'B	�FB	�?B	�RB	�dB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ɺB	ƨB	ĜB	ÖB	ŢB	B	��B	�
B	�B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�BB	�HB	�BB	�HB	�TB	�TB	�BB	�/B	�#B	�B	�
B	�
B	�B	�B	�;B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�yB	�`B	�yB	�B	�NB	�BB	�HB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�fB	�TB	�BB	�BB	�HB	�NB	�`B	�fB	�fB	�mB	�mB	�mB	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
1B
{B
�B
�B
�B
%�B
/B
49B
9XB
A�B
H�B
M�B
O�B
S�B
[#B
_;B
cTB
gmB
jB
m�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
�:B
�9B
�*B
ʻB
ȭB
��B
��B
��B
�B
�B
�B
�B
�5B
�{B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�QBB6<BC�B:UB69BZB��B˻B��B�}B�B3%BG�BG�BH�BL�B`7Br�B�B�B�B�KB�B�B�B)B�B�B�B�B�B�B�BBeBjBAB��B�B�vBB\B~B'B��B�B�NBƙB��BilBD�BB��B��B�%B��Bu�Bp�Bn�B��B�sB�$B�cBx�Bg_B] BK�BE�BK�BR�BE�B �B$�B�BWBB
��B
�B
��B
{�B
s�B
juB
\B
F�B
bB	��B	ȰB	�	B	��B	�AB	w�B	aGB	P�B	A�B	36B	!�B	�B	�B	uB		:B��B�vB�"B��BɾBB�[B�>B�8B�B��B��B��B�uB��B�	B�B�B��B��B��B��B�B�5B�B��B��B��B��B��B�B�<B�)B��B��B�NB|�Bz�Bv�Bt�B�"B�mB��B�cB�qBBɻBɻB��B�*B��B		6B		7B	*B	B��B��B��B��B�B��B��B	B	CB	dB	[B	�B	#�B	6?B	=nB	L�B	ZB	^2B	bJB	dVB	`=B	\$B	cOB	_7B	WB	R�B	R�B	S�B	_7B	l�B	q�B	n�B	l�B	l�B	s�B	v�B	u�B	p�B	ggB	aBB	_6B	[B	S�B	F�B	1%B	0B	+ B	#�B	5:B	8MB	?zB	F�B	E�B	E�B	C�B	D�B	L�B	N�B	N�B	R�B	WB	ZB	O�B	J�B	L�B	WB	cOB	q�B	v�B	v�B	z�B	}�B	�B	~�B	|�B	z�B	{�B	{�B	|�B	|�B	{�B	y�B	y�B	y�B	y�B	�1B	�CB	�fB	�hB	�IB	�B	~�B	�B	�B	{�B	x�B	w�B	r�B	n�B	k�B	jzB	l�B	p�B	z�B	�B	�B	{�B	�6B	�]B	�B	o�B	X	B	S�B	P�B	P�B	WB	^.B	hmB	z�B	{�B	�KB	�B	�yB	�hB	�mB	��B	��B	��B	�zB	�mB	��B	��B	��B	��B	��B	��B	��B	�B	�UB	�TB	�^B	�dB	�\B	�>B	�B	��B	��B	��B	�B	�B	�B	�<B	�5B	�IB	�\B	ŗB	��B	��B	��B	��B	��B	��B	��B	��B	˽B	ɰB	ɰB	ƝB	ĐB	ÍB	ŘB	B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�)B	�7B	�=B	�:B	�;B	�KB	�JB	�9B	�(B	�B	�B	�B	�B	�B	�B	�1B	�kB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�oB	�VB	�oB	�rB	�CB	�7B	�>B	�BB	�KB	�JB	�RB	�UB	�]B	�eB	�\B	�JB	�9B	�9B	�?B	�EB	�VB	�\B	�\B	�aB	�bB	�bB	�yB	�yB	�B	�nB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
 B
	B
B
B
B
B
G�O�B
&B
nB
�B
uB
�B
%�B
/B
4-B
9KB
A}B
H�B
M�B
O�B
S�B
[B
_.B
cFB
g_B
jpB
m�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.14 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436382016080714363820160807143638  AO  ARCAADJP                                                                    20150826191806    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150826191806  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150826191806  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143638  IP                  G�O�G�O�G�O�                