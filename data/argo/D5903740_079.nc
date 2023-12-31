CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:46Z AOML 3.0 creation; 2016-06-01T00:08:18Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230846  20160531170818  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               OA   AO  4055_7112_079                   2C  D   APEX                            5374                            041511                          846 @��8����1   @��9?V?�@:�S����d��v�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    OA   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy` D��D�L�D�y�D���D��D�<�D�|�D��3D�3D�S3D���DǼ�D�  D�C3D�|�D��3D�	�D�0 D�l�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A��A$��AD��Ad��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB	=qB=qB=qB!=qB)=qB1=qB9=qBA=qBI=qBQ=qBY=qBa=qBi=qBq=qBy=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B̞�BО�BԞ�B؞�Bܞ�B���B䞸B螸B잸B�B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\CO\CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\CrO\CtO\CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dtz=Dys�D��D�V�D���D�ÆD�#�D�F�D���D��D�D�]D���D�ƹD�)�D�MDچ�D��D��D�9�D�v�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��PA�XA���A��9A���A��+A�r�A�bNA�M�A�=qA�1'A��A��A���A���A�bA�A�A��`A��A�  A���A�%A���A���A�oA���A�=qA���A� �A��;A��A��A�XA��A��A�x�A��A��A�z�A�1A��HA���A�dZA�(�A��+A�A�A�(�A��A��A�O�A���A�JA��A��A�A�A�Q�A��A�G�A�ĜA�dZA��A��;A���A��7A���A�7LA�ƨA�dZA���A��A���A�JA�l�A�XA�%A��HA���A�bNA��TA�A�A�I�A�1'A�VA�ffA��jA�A��hA��A�9XA��A���A��^A���A�hsA~�HAy�7Aw33Au33As�-Ar��Aq�FAq%Ao�TAnQ�Aj�`Ag��Ag�
Ae��Ad��Ac��Ab��AbI�Aa��Aap�A_x�A^�A\1'A[?}AZ��AY��AX�AV(�AS�#ARAPAN�RANbAMp�AL��AK�7AJ�yAJĜAJ^5AJ �AI�AH��AHv�AHn�AH5?AHAG�FAGS�AFA�AE&�AC�wAB�jA@�A>r�A=�A=7LA<�uA<5?A;ƨA;?}A:�A:I�A:$�A9�7A7�wA5��A3+A1��A1��A1S�A0~�A.��A-dZA,�A,ȴA,��A,�DA,ffA,I�A,(�A+�A+��A+t�A*�RA*��A)��A)|�A)G�A(�A'�wA&��A&A�A&A%��A$�A$��A#A"ĜA �uA��A5?A%A��A{A�`AQ�AAQ�A��A��AĜAVAM�AQ�A1'A��AVA�hAVA��A�A��AjA1'A �AAp�A
=A	��A	t�A��A��A`BAjA��AdZA n�@��@�Q�@��@��!@�I�@�K�@��\@��@��j@�^5@�&�@�9X@���@��@�@�M�@�X@���@���@�I�@�R@�hs@��@���@��`@�(�@�=q@���@���@���@�-@�p�@���@�+@܃@��;@���@١�@�G�@���@�33@�/@�dZ@��/@�"�@̃@˝�@ʸR@�$�@�@�@���@�bN@��@�  @��@��
@�ƨ@ǍP@�S�@ƸR@�{@��@�+@�?}@���@��@��@�Z@��@��@��m@��F@��P@�dZ@�;d@��@�ȴ@�$�@��@��-@�hs@�Ĝ@�A�@�1'@��@���@�\)@��!@�^5@�=q@���@��-@���@� �@��@���@�?}@�V@��/@�I�@�33@��@�G�@�z�@�Z@�9X@�b@��
@���@���@���@��m@��@���@�n�@���@�1@�ȴ@��#@�%@�I�@��@�{@���@���@�hs@�Z@�
=@��h@��/@��D@��
@�J@���@�Z@�1@�ƨ@��@���@�|�@�S�@�C�@�C�@�o@�ȴ@�X@��@��/@�Ĝ@�Ĝ@��j@��j@��j@��@��u@��D@��D@�  @�"�@��y@��\@�ff@�V@�$�@�@�p�@�&�@�V@��@�A�@��P@���@��+@�5?@�-@�$�@�-@�-@�{@�{@�{@�{@�{@�{@�@��7@��@��@�Q�@�1'@�b@��m@��w@�S�@��+@�E�@�5?@�J@��7@�7L@�%@��D@��
@�"�@�@���@�@�@�@�@�@�@��@��H@�^5@�{@��@��@��@�r�@�I�@;d@~5?@}O�@|�@|��@|��@|z�@|Z@|(�@{��@{ƨ@{ƨ@{��@{t�@{@zM�@y��@y��@yX@x��@xr�@w�w@w+@v5?@u�@t�@t�D@t9X@s��@st�@sC�@sdZ@s��@s�@r�\@o+@i��@ep�@\�@Up�@M�@G\)@C�@:��@4�@0Q�@+dZ@%�@!�@�+@x�@/@�7@�@C�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��PA�XA���A��9A���A��+A�r�A�bNA�M�A�=qA�1'A��A��A���A���A�bA�A�A��`A��A�  A���A�%A���A���A�oA���A�=qA���A� �A��;A��A��A�XA��A��A�x�A��A��A�z�A�1A��HA���A�dZA�(�A��+A�A�A�(�A��A��A�O�A���A�JA��A��A�A�A�Q�A��A�G�A�ĜA�dZA��A��;A���A��7A���A�7LA�ƨA�dZA���A��A���A�JA�l�A�XA�%A��HA���A�bNA��TA�A�A�I�A�1'A�VA�ffA��jA�A��hA��A�9XA��A���A��^A���A�hsA~�HAy�7Aw33Au33As�-Ar��Aq�FAq%Ao�TAnQ�Aj�`Ag��Ag�
Ae��Ad��Ac��Ab��AbI�Aa��Aap�A_x�A^�A\1'A[?}AZ��AY��AX�AV(�AS�#ARAPAN�RANbAMp�AL��AK�7AJ�yAJĜAJ^5AJ �AI�AH��AHv�AHn�AH5?AHAG�FAGS�AFA�AE&�AC�wAB�jA@�A>r�A=�A=7LA<�uA<5?A;ƨA;?}A:�A:I�A:$�A9�7A7�wA5��A3+A1��A1��A1S�A0~�A.��A-dZA,�A,ȴA,��A,�DA,ffA,I�A,(�A+�A+��A+t�A*�RA*��A)��A)|�A)G�A(�A'�wA&��A&A�A&A%��A$�A$��A#A"ĜA �uA��A5?A%A��A{A�`AQ�AAQ�A��A��AĜAVAM�AQ�A1'A��AVA�hAVA��A�A��AjA1'A �AAp�A
=A	��A	t�A��A��A`BAjA��AdZA n�@��@�Q�@��@��!@�I�@�K�@��\@��@��j@�^5@�&�@�9X@���@��@�@�M�@�X@���@���@�I�@�R@�hs@��@���@��`@�(�@�=q@���@���@���@�-@�p�@���@�+@܃@��;@���@١�@�G�@���@�33@�/@�dZ@��/@�"�@̃@˝�@ʸR@�$�@�@�@���@�bN@��@�  @��@��
@�ƨ@ǍP@�S�@ƸR@�{@��@�+@�?}@���@��@��@�Z@��@��@��m@��F@��P@�dZ@�;d@��@�ȴ@�$�@��@��-@�hs@�Ĝ@�A�@�1'@��@���@�\)@��!@�^5@�=q@���@��-@���@� �@��@���@�?}@�V@��/@�I�@�33@��@�G�@�z�@�Z@�9X@�b@��
@���@���@���@��m@��@���@�n�@���@�1@�ȴ@��#@�%@�I�@��@�{@���@���@�hs@�Z@�
=@��h@��/@��D@��
@�J@���@�Z@�1@�ƨ@��@���@�|�@�S�@�C�@�C�@�o@�ȴ@�X@��@��/@�Ĝ@�Ĝ@��j@��j@��j@��@��u@��D@��D@�  @�"�@��y@��\@�ff@�V@�$�@�@�p�@�&�@�V@��@�A�@��P@���@��+@�5?@�-@�$�@�-@�-@�{@�{@�{@�{@�{@�{@�@��7@��@��@�Q�@�1'@�b@��m@��w@�S�@��+@�E�@�5?@�J@��7@�7L@�%@��D@��
@�"�@�@���@�@�@�@�@�@�@��@��H@�^5@�{@��@��@��@�r�@�I�@;d@~5?@}O�@|�@|��@|��@|z�@|Z@|(�@{��@{ƨ@{ƨ@{��@{t�@{@zM�@y��@y��@yX@x��@xr�@w�w@w+@v5?@u�@t�@t�D@t9X@s��@st�@sC�@sdZ@s��@s�@r�\@o+@i��@ep�@\�@Up�@M�@G\)@C�@:��@4�@0Q�@+dZ@%�@!�@�+@x�@/@�7@�@C�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�qB�^B�?B�3B�-B�'B�!B�!B�B�B�B��B��B��B�7BXBK�BH�B:^B,B�B�B�B�B�BhBDBB��B��B�B�mB�/B�
B��BɺB��B�!B��B��B��B�oB�PB�1B}�By�Bz�Bq�Be`BP�BE�B:^B<jB8RB/B{BB��B�yB�;B�B�
B��B��B�FB��B�JB�Bt�B[#BB�B&�B�B�BuBbBPBB
��B
�B
�#B
B
�B
�uB
�=B
~�B
}�B
�DB
�=B
�B
�B
�B
|�B
t�B
^5B
,B
uB
  B	�B	�sB	�;B	�B	��B	�jB	��B	�+B	�B	u�B	o�B	l�B	iyB	hsB	hsB	x�B	� B	y�B	s�B	p�B	m�B	hsB	`BB	W
B	M�B	E�B	>wB	9XB	7LB	49B	1'B	-B	+B	)�B	(�B	&�B	$�B	"�B	!�B	 �B	�B	�B	�B	�B	�B	bB	DB	B��B��B��B�B�B�B�B�B�yB�mB�`B�HB�#B��B��BɺBȴBƨBB�wB�^B�XB�XB�RB�RB�LB�LB�FB�?B�9B�3B�'B�!B�B�B�B��B��B��B��B��B��B��B��B��B�hB�DB�1B�B�B�B}�By�Bu�Br�Bk�B^5B[#BYBXBW
BW
BT�BR�BO�BM�BJ�BH�BF�BF�BE�BE�BC�BC�BB�B@�B>wB<jB8RB5?B33B1'B.B-B)�B+B+B)�B(�B(�B(�B(�B'�B&�B'�B'�B'�B&�B&�B&�B%�B%�B$�B"�B#�B%�B)�B)�B)�B)�B(�B-B-B,B,B,B+B)�B)�B,B,B,B,B-B,B+B+B+B2-B33B7LB;dB>wBA�BA�BB�BE�BH�BI�BJ�BJ�BK�BK�BM�BM�BO�BP�BP�BP�BT�BVBVBT�BS�BT�BZBbNBcTBdZBdZBe`Be`BffBjBjBl�Bl�Bm�Bp�Bp�Bp�Bp�Bq�Bs�Bs�Bs�Bt�Bt�Bu�Bw�Bz�B|�B~�B~�B~�B~�B� B~�B}�B|�B|�B|�B|�B|�B|�B|�B�B�B�B�B�B�=B�VB�hB�uB��B��B��B��B��B��B��B�B�'B�FB�XB�XB�dBĜB��B��B��B��B��B��B��B�B�B�B�B�/B�sB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	B	B	B	%B		7B	\B	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	&�B	+B	-B	-B	.B	/B	0!B	33B	9XB	;dB	;dB	<jB	@�B	C�B	D�B	H�B	N�B	VB	XB	XB	YB	YB	YB	YB	YB	YB	YB	YB	\)B	^5B	_;B	cTB	e`B	ffB	ffB	l�B	s�B	x�B	z�B	{�B	{�B	{�B	|�B	}�B	~�B	� B	� B	�B	�B	�B	�%B	�+B	�+B	�1B	�1B	�7B	�7B	�7B	�DB	�PB	�\B	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	�XB	ȴB	�NB	�B
B
bB
�B
+B
49B
;dB
A�B
I�B
O�B
P�B
XB
^5B
dZB
iyB
l�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�]B�FB�+B�B�B�B�B�	B�B��B��B��B��B��B�BW�BK�BH�B:DB+�B�B�B�B�BtBKB%B �B��B��B�B�RB�B��B��BɟB�kB�B��B��B�{B�QB�3B�B}�By�Bz�Bq�BeABP�BE�B:AB<KB86B/ B]B�B��B�[B�B� B��B��B˩B�)B�jB�+B��Bt�B[BBrB&�B�B�BXBDB3B�B
��B
�nB
�B
�uB
��B
�ZB
�B
~�B
}�B
�)B
�#B
�B
��B
��B
|�B
t�B
^B
+�B
]B	��B	�B	�[B	�"B	��B	̶B	�UB	��B	�B	�B	u�B	o�B	lvB	idB	hbB	hbB	x�B	�B	y�B	s�B	p�B	m}B	h_B	`0B	V�B	M�B	E�B	>fB	9EB	7:B	4'B	1B	,�B	*�B	)�B	(�B	&�B	$�B	"�B	!�B	 �B	�B	�B	�B	�B	oB	PB	3B	B��B��B��B�B�B�B�B�sB�iB�[B�MB�7B�B��B̻BɧBȦBƗBB�gB�OB�HB�IB�AB�CB�<B�=B�7B�0B�(B�"B�B�B�B��B��B��B��B��B��B��B��B��B��B�vB�\B�6B�$B�B�B��B}�By�Bu�Br�BkxB^)B[BYBXBV�BV�BT�BR�BO�BM�BJ�BH�BF�BF�BE�BE�BC�BC�BB�B@tB>iB<_B8EB53B3)B1B.
B-B)�B*�B*�B)�B(�B(�B(�B(�B'�B&�B'�B'�B'�B&�B&�B&�B%�B%�B$�B"�B#�B%�B)�B)�B)�B)�B(�B- B- B+�B+�B+�B*�B)�B)�B+�B+�B+�B+�B- B+�B*�B*�B*�B2B3&B7>B;TB>hBAzBAzBB�BE�BH�BI�BJ�BJ�BK�BK�BM�BM�BO�BP�BP�BP�BT�BU�BU�BT�BS�BT�BZBb=Bc@BdGBdHBeNBeMBfUBjoBjkBlyBltBm}Bp�Bp�Bp�Bp�Bq�Bs�Bs�Bs�Bt�Bt�Bu�Bw�Bz�B|�B~�B~�B~�B~�B�B~�B}�B|�B|�B|�B|�B|�B|�B|�B��B�B�B�B�
B�(B�AB�UB�_B�yB��B��B��B��B��B��B��B�B�/B�AB�AB�LBĆB̵B��B��B��B��B��B��B��B��B��B�B�B�XB�mB�kB�sB�rB�sB�rB�rB�sB�sB�qB�nB�B��B��B��B��B��B��B��B	�B	B	B	B		B	BB	YB	pB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	&�B	*�B	,�B	,�B	-�B	.�B	0B	3B	98B	;HB	;IB	<MB	@gB	CyB	D�B	H�B	N�B	U�B	W�B	W�B	X�B	X�B	X�B	X�B	X�B	X�B	X�B	X�B	\	B	^B	_B	c5B	eAB	fGB	fGB	ljB	s�B	x�B	z�B	{�B	{�B	{�B	|�B	}�B	~�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�/B	�<B	�CB	�JB	�NB	�\B	�uB	�B	��B	��B	��B	��B	�8B	ȓB	�,B	�B
�B
BB
�B
*�B
4B
;DB
AgB
I�B
O�B
P�B
W�B
^B
d5B
iVB
lhB
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.31 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708182016053117081820160531170818  AO  ARCAADJP                                                                    20140721230846    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230846  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230846  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170818  IP                  G�O�G�O�G�O�                