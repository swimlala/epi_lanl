CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:27Z AOML 3.0 creation; 2016-06-01T00:08:12Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230827  20160531170812  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               +A   AO  4055_7112_043                   2C  D   APEX                            5374                            041511                          846 @֘�m�?�1   @֘���@:.��O�;�cj��`A�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    +A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy�3D��D�@ D�|�D��fD�3D�FfD�|�D���D� D�@ D�y�D���D��D�9�D�s3D�ٚD��D�9�D�i�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A��A$��AD��Ad��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB	=qB=qB=qB!=qB)=qB1=qB9=qBA=qBI=qBQ=qBY=qBa=qBi=qBq=qBy=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B̞�BО�BԞ�B؞�Bܞ�B���B䞸B螸B잸B�B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\CO\CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTh�CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\CrO\CtO\CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-=D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy�
D�&�D�I�D���D��RD�D�PRD���D�ֹD��D�I�D���D��D��D�C�D�}D��D��D�C�D�s�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��PA���A���A���A���A��A��A���A���A�t�A�l�A�n�A�l�A�hsA�dZA�bNA�`BA�bNA�\)A�Q�A��
A��;A�O�A�hsA���A��A�|�A�XA�9XA��^A�;dA�~�A��mA�E�A�A��A���A�E�A���A�C�A��`A��A��A��A�t�A�?}A�
=A��A��PA��A�E�A��PA�K�A��
A�`BA���A�jA�|�A�A�JA�  A���A���A�JA�ĜA�1A��\A�?}A�ȴA�"�A��^A���A�ƨA�Q�A�%A�$�A�\)A�1'A�K�A�ZA���A��;A�ȴA��A��A�hsA�A�$�A�\)A��DA���A��hA�ȴA��;A��A�-A�hsA�`BA�/A�A�7LA�S�A�A�A}�FA|ffA{oAx��AxA�Au�AsAp�uAo�PAn�Al��Ai`BAg"�Ad �Aat�A_��A_%A^ZA]�A\�A[G�AZE�AW�AV{AU?}AT-ASG�ARz�AQ��AP�APVAOl�AM+AKXAI�7AI�7AH��AH�\AIVAIp�AI7LAH�AF��AEXAEVAD�+ABQ�A@�!A?�PA>r�A=��A=`BA<��A<�9A<E�A;��A9S�A7�A7�A5�TA4�`A41'A3"�A2jA1G�A0=qA.�`A.VA.E�A.I�A.(�A-��A-l�A,��A+�FA+�A+S�A+33A+oA*��A*�yA*��A)�mA(=qA%&�A#�
A"�A"VA!�A jA��A=qA��A�jA1AAA��A{A��AffA33A(�A�FA��AK�A�uA�hA�A�`A5?A�A-AVA
ZA	�A5?AC�AZA��A|�Az�A/@��@��7@�G�@��D@�ƨ@���@�hs@�V@�9X@�33@��H@�@��
@�dZ@���@�~�@���@웦@�\)@�=q@�X@�A�@�o@���@��/@�(�@�F@���@��@��@�|�@�&�@�I�@�ƨ@�|�@���@أ�@��@��@��#@�"�@�@���@�x�@�Z@���@�%@�  @ˍP@�@�$�@�G�@ȋD@�b@Ɵ�@��@�7L@ļj@�|�@�@��@���@��m@��@�@��T@�j@���@��H@�?}@�r�@�(�@��;@���@��@�hs@��j@���@�|�@��\@�M�@�@��7@��u@�|�@�ȴ@�-@���@�O�@�A�@�\)@���@�^5@�{@���@��D@�  @�
=@���@���@���@�E�@���@�7L@�Z@��w@���@��y@��\@�J@��^@�p�@��/@�b@��@�"�@�V@���@��@�%@���@�Q�@�33@��!@�=q@��-@��/@�I�@�(�@��@���@�l�@�
=@���@�V@���@��h@�p�@��/@�9X@���@���@�V@�@���@�@��-@�G�@��`@��j@���@�9X@�  @��
@�dZ@�o@��y@���@�v�@�$�@���@��^@�x�@�&�@�V@���@��@���@�1'@��
@�K�@�o@�
=@��@���@�M�@���@��#@��-@�X@�%@�r�@�I�@�9X@�1@���@�l�@�S�@��@���@��H@��R@��!@���@�n�@�V@�J@���@��h@��@�p�@�X@�O�@�%@���@��D@�z�@�z�@�Z@�(�@�1@���@��@�"�@��R@��+@�^5@�-@�{@���@��^@�`B@�V@��`@��@��u@�j@�A�@��@�  @�@�@~�@~�R@~��@~�+@~ff@~@}@}O�@|�@|�@|z�@|j@|Z@|I�@|1@{dZ@z��@zn�@zn�@z^5@zM�@y�@y��@y�7@y�7@yx�@yG�@x�`@x��@x��@uV@m/@e�@\�@U�@P1'@H��@Bn�@<�@7\)@2�!@,�j@%`B@!&�@I�@�@��@b@�@
=q@ȴ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��PA���A���A���A���A��A��A���A���A�t�A�l�A�n�A�l�A�hsA�dZA�bNA�`BA�bNA�\)A�Q�A��
A��;A�O�A�hsA���A��A�|�A�XA�9XA��^A�;dA�~�A��mA�E�A�A��A���A�E�A���A�C�A��`A��A��A��A�t�A�?}A�
=A��A��PA��A�E�A��PA�K�A��
A�`BA���A�jA�|�A�A�JA�  A���A���A�JA�ĜA�1A��\A�?}A�ȴA�"�A��^A���A�ƨA�Q�A�%A�$�A�\)A�1'A�K�A�ZA���A��;A�ȴA��A��A�hsA�A�$�A�\)A��DA���A��hA�ȴA��;A��A�-A�hsA�`BA�/A�A�7LA�S�A�A�A}�FA|ffA{oAx��AxA�Au�AsAp�uAo�PAn�Al��Ai`BAg"�Ad �Aat�A_��A_%A^ZA]�A\�A[G�AZE�AW�AV{AU?}AT-ASG�ARz�AQ��AP�APVAOl�AM+AKXAI�7AI�7AH��AH�\AIVAIp�AI7LAH�AF��AEXAEVAD�+ABQ�A@�!A?�PA>r�A=��A=`BA<��A<�9A<E�A;��A9S�A7�A7�A5�TA4�`A41'A3"�A2jA1G�A0=qA.�`A.VA.E�A.I�A.(�A-��A-l�A,��A+�FA+�A+S�A+33A+oA*��A*�yA*��A)�mA(=qA%&�A#�
A"�A"VA!�A jA��A=qA��A�jA1AAA��A{A��AffA33A(�A�FA��AK�A�uA�hA�A�`A5?A�A-AVA
ZA	�A5?AC�AZA��A|�Az�A/@��@��7@�G�@��D@�ƨ@���@�hs@�V@�9X@�33@��H@�@��
@�dZ@���@�~�@���@웦@�\)@�=q@�X@�A�@�o@���@��/@�(�@�F@���@��@��@�|�@�&�@�I�@�ƨ@�|�@���@أ�@��@��@��#@�"�@�@���@�x�@�Z@���@�%@�  @ˍP@�@�$�@�G�@ȋD@�b@Ɵ�@��@�7L@ļj@�|�@�@��@���@��m@��@�@��T@�j@���@��H@�?}@�r�@�(�@��;@���@��@�hs@��j@���@�|�@��\@�M�@�@��7@��u@�|�@�ȴ@�-@���@�O�@�A�@�\)@���@�^5@�{@���@��D@�  @�
=@���@���@���@�E�@���@�7L@�Z@��w@���@��y@��\@�J@��^@�p�@��/@�b@��@�"�@�V@���@��@�%@���@�Q�@�33@��!@�=q@��-@��/@�I�@�(�@��@���@�l�@�
=@���@�V@���@��h@�p�@��/@�9X@���@���@�V@�@���@�@��-@�G�@��`@��j@���@�9X@�  @��
@�dZ@�o@��y@���@�v�@�$�@���@��^@�x�@�&�@�V@���@��@���@�1'@��
@�K�@�o@�
=@��@���@�M�@���@��#@��-@�X@�%@�r�@�I�@�9X@�1@���@�l�@�S�@��@���@��H@��R@��!@���@�n�@�V@�J@���@��h@��@�p�@�X@�O�@�%@���@��D@�z�@�z�@�Z@�(�@�1@���@��@�"�@��R@��+@�^5@�-@�{@���@��^@�`B@�V@��`@��@��u@�j@�A�@��@�  @�@�@~�@~�R@~��@~�+@~ff@~@}@}O�@|�@|�@|z�@|j@|Z@|I�@|1@{dZ@z��@zn�@zn�@z^5@zM�@y�@y��@y�7@y�7@yx�@yG�@x�`@x��@x��@uV@m/@e�@\�@U�@P1'@H��@Bn�@<�@7\)@2�!@,�j@%`B@!&�@I�@�@��@b@�@
=q@ȴ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B�oB�oB�oB�uB�hB�oB�uB�uB��B�{B�{B��B��B��B��B�-B��B��B��B��BɺBƨBB�}B�qB�jB�dB�^B�^B�qB�dB�^B�RB�XB�FB�3B�3B�!B�B��B��B�JB�B}�Bs�BiyB_;BP�B;dB2-B7LB6FB,B�B1B�mB��BŢB�jB�-B��B�BjB[#BYBXB>wB#�B+B�B�NB��B��B�bB{�BgmBW
BH�B;dB.B�BPB
��B
�B
�B
ȴB
�^B
�B
��B
�7B
s�B
gmB
VB
9XB
!�B
�B

=B	��B	�B	�;B	��B	�dB	�-B	�B	��B	�B	t�B	`BB	M�B	B�B	=qB	8RB	33B	-B	$�B	�B	�B	PB	DB	JB	�B	�B	�B	�B	"�B	(�B	�B	DB	B	B	%B		7B	{B	�B	!�B	�B	uB	PB	DB	\B	B��B�B�B�B�B�B�B�B�B�#B��B��B��BɺBǮBŢBB�XB�-B�B�B�B�!B�'B�!B�B�B��B��B��B��B��B��B��B��B��B�\Bz�Bv�Bs�Bp�Bl�BiyBffBbNB_;B]/BZBW
BR�BO�BM�BL�BK�BG�BE�BB�B?}B<jB:^B9XB7LB6FB5?B33B/B-B)�B%�B#�B#�B"�B�B�B�B{BoBoBhBbB\BVBJB
=B
=B
=B	7B1B1B1B+B+B%B%B%B%BBBBBBBBBBBB%B%B%B%B+B1B1B1B1BJBVBVBVBVB\BoB{B{B{B�B�B�B�B�B�B�B�B�B�B"�B!�B#�B#�B#�B%�B'�B)�B)�B.B/B0!B0!B0!B1'B49B6FB7LB8RB:^B;dB;dB<jB>wB@�BB�BD�BE�BF�BH�BK�BM�BM�BN�BO�BS�BT�BYB[#BZBZB[#B]/B_;BcTBffBffBiyBjBm�Bn�Bo�Bq�Bv�By�Bz�B~�B�B�B�%B�+B�7B�bB�uB��B��B��B��B��B��B��B��B�B�B�!B�3B�9B�?B�RB�qB�}BĜBɺB��B��B��B��B��B��B�B�
B�#B�5B�5B�TB�fB�mB�yB�B�B�B�B�B��B��B��B��B��B��B��B	B	B	B	%B	
=B	PB	\B	bB	hB	oB	�B	�B	�B	�B	�B	"�B	$�B	%�B	(�B	)�B	+B	-B	-B	.B	0!B	1'B	6FB	:^B	<jB	<jB	=qB	=qB	>wB	@�B	B�B	D�B	E�B	F�B	G�B	I�B	J�B	M�B	P�B	R�B	VB	W
B	W
B	YB	YB	ZB	\)B	`BB	dZB	ffB	hsB	iyB	iyB	k�B	m�B	n�B	p�B	s�B	t�B	u�B	v�B	v�B	w�B	y�B	{�B	~�B	�B	�B	�B	�B	�B	�B	�+B	�=B	�VB	�\B	�bB	�hB	�hB	�oB	�{B	�{B	�{B	��B	��B	��B	��B	��B	�B	B	�;B	��B
B
bB
�B
&�B
2-B
8RB
>wB
F�B
N�B
T�B
\)B
aHB
e`B
iyB
m�B
q�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�wB�qB�qB�tB�xB�xB�xB�xB�tB�YB�YB�YB�bB�QB�\B�bB�bB�nB�gB�gB�wB��B��B��B�B��B��B��B;BɢBƓB�yB�iB�]B�VB�MB�GB�HB�YB�LB�BB�=B�BB�1B�B�B�B��B��B��B�5B��B}�Bs�BicB_"BP�B;GB2B73B6-B+�B�BB�QB��BŅB�PB�B��B��BjfB[BX�BW�B>\B#�BB�B�2B��B��B�DB{�BgRBV�BH�B;IB-�B�B4B
��B
�gB
�B
ȗB
�CB
��B
��B
�B
s�B
gSB
U�B
9?B
!�B
gB

(B	��B	�B	�&B	��B	�OB	�B	��B	��B	�B	t�B	`/B	M�B	B{B	=\B	8?B	3"B	,�B	$�B	�B	qB	@B	2B	9B	oB	{B	�B	�B	"�B	(�B	{B	1B	B	B	B		&B	kB	�B	!�B	�B	`B	>B	1B	LB	 �B��B�B�B�~B�B�B�~B�B�uB�B��B��B��BɩBǛBőBB�FB�B��B� B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�PBz�Bv�Bs�Bp�Bl|BioBfYBb?B_.B]BZBV�BR�BO�BM�BL�BK�BG�BE�BB�B?pB<^B:SB9NB7'B6:B53B3)B/B-B)�B%�B#�B#�B"�B�B{BbBpBGBJBAB<B6B/B$B
B
B
B	BB%B
BBB�B�B BB�B�B�B�B�B�B�B�B�B�B�B�B�BB�BBB$BB
B"B/B.B.B-BOBbBoBSBnBVB`B�B}B�B�B~BB�B�B"�B!�B#�B#�B#�B%�B'�B)�B)�B.B/B0B0B0B1B4'B66B7;B8DB:OB;QB;QB<[B>fB@sBB~BD�BE�BF�BH�BK�BM�BM�BN�BO�BS�BT�BYB[BZ	BZB[B]B_(BcABfRBfSBigBjmBm�Bn�Bo�Bq�Bv�By�Bz�B~�B��B� B�B�B�$B�MB�`B�sB��B��B��B��B��B��B��B��B��B�B�B�#B�*B�9B�XB�dBćBɡBˬB̶BͺBͺB��B��B��B��B�B�B�B�:B�JB�UB�`B�hB�vB�~B��B�B��B��B��B��B��B��B��B	�B	B	B	
B	
 B	5B	BB	GB	NB	TB	eB	�B	�B	�B	�B	"�B	$�B	%�B	(�B	)�B	*�B	,�B	,�B	-�B	0B	1B	6+B	:AB	<LB	<NB	=WB	=UB	>XB	@gB	BrB	D�B	E�B	F�B	G�B	I�B	J�B	M�B	P�B	R�B	U�B	V�B	V�B	X�B	X�B	Y�B	\B	`$B	d=B	fHB	hUB	i[B	iXB	khB	mtB	nyB	p�B	s�B	t�B	u�B	v�B	v�B	w�B	y�B	{�B	~�B	��B	��B	�B	�B	� B	� B	�B	�B	�8B	�<B	�DB	�GB	�HB	�MB	�\B	�\B	�^B	�`B	�mB	�B	��B	��B	��B	�mB	�B	��B
�B
?B
�B
&�B
2B
8.B
>RB
F�B
N�B
T�B
\B
a#B
e;B
iUB
mmB
q�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.31 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708122016053117081220160531170812  AO  ARCAADJP                                                                    20140721230827    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230827  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230827  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170812  IP                  G�O�G�O�G�O�                