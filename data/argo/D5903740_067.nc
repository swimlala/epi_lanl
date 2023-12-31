CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:40Z AOML 3.0 creation; 2016-06-01T00:08:16Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230840  20160531170816  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               CA   AO  4055_7112_067                   2C  D   APEX                            5374                            041511                          846 @��r~�1   @��sӀ@;[�l�C��d&��"��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    CA   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.fD.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D8��D9y�D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy�fD��D�VfD�� D�ٚD���D�L�D�p D�ٚD�  D�9�D��fD��3D���D�0 Dڃ3D�ٚD� D�S3D� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�ffA33A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.3D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9gD9�gD:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt� Dy�3D�3D�\�D��fD�� D�3D�S3D�vfD�� D�fD�@ D���D�əD�  D�6fDډ�D�� D�fD�Y�D�fD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�^5A�`BA�`BA�dZA�bNA�bNA�ffA�bNA�ffA�hsA�jA�ffA�ffA�ffA�ffA�jA�ffA�ffA�ffA�ffA�hsA�jA�bNA�`BA�dZA�`BA�`BA�^5A�^5A�^5A�`BA�dZA�ffA�jA�l�A�l�A�n�A�x�A�x�A�v�A�v�A�z�A�|�A�|�A�~�A�~�A�|�A�~�A�~�A�x�A�`BA�bNA�dZA�p�A�M�A�VA���A��PA�ƨA���A���A�A��#A��7A�/A��A�M�A���A���A��A���A��DA�1A���A��#A���A�`BA��wA�;dA��A�%A�ĜA��A�bA�7LA���A��hA�A��A��A���A�oA�t�A�n�A�VA�5?A�=qA���A�;dA���A��mA��;A�33A�A�A�M�A�ffA��A�7A}�
A{�
Az1'Az5?Ay�Au��ArVApVAm�FAk�AjM�AiC�AhjAhbAg�-AgG�AfJAc�PAa�^A^�A\�uAZĜAY�AX=qAW+AV�9ATĜAR9XAQ\)AP��API�AO�AMl�AL �AK\)AJ�`AH�\AE33ACƨABĜA?ƨA<�A<��A<1'A;��A;�;A;ƨA;�A;��A;��A;��A;��A;G�A;?}A:�yA:��A:�A:�DA:jA:A�A:�A9�TA7O�A2^5A.�A-G�A,bNA,1A+��A+?}A++A*�A*^5A)��A)|�A)p�A)S�A)7LA)�A(��A(�HA(ĜA(��A(~�A(ffA(E�A(1'A(  A'|�A&��A&�9A&�DA&bA%K�A%"�A$�`A$�9A#��A"�A!+A��AA`BA �A�\A$�AXAI�A�;Al�A1A1A �A\)A/A�A��A+A$�A
=A
VA	�PA	XA	7LA	oA�A��A^5A�A��A�jA��A1'AbA�A��A��AO�A%A �R@��@��y@��u@�@�@�M�@�@�ƨ@@�;d@��@��@�Ĝ@땁@�+@�1@��H@�?}@�M�@��/@�I�@�1'@��
@�;d@�v�@�@�?}@�1'@�O�@���@�dZ@�G�@���@�bN@��@ύP@�ȴ@�J@�hs@̓u@�o@ț�@�\)@�=q@�I�@��H@���@���@��@�ȴ@�^5@��#@�Z@���@���@�J@�G�@��@�r�@���@��@��@��!@�5?@���@���@�r�@�ƨ@�\)@���@�n�@�E�@�{@��@��^@���@�hs@�%@�z�@���@��P@��@�M�@���@�p�@�%@�j@���@��@���@�v�@��-@��@�;d@�ff@��@�1'@�t�@�dZ@�C�@���@�M�@��@���@��^@���@�X@�&�@��@��9@�9X@��m@�l�@��y@�V@��T@�hs@�7L@��@���@�;d@�+@�+@�
=@�V@��h@�p�@�O�@��@���@��@�^5@���@��h@��7@�x�@�x�@�?}@��@��/@��@�bN@�b@�  @���@���@���@���@��@���@�ƨ@�+@�@��h@�p�@�hs@�X@�G�@�/@��@��j@��u@��@�j@�A�@�b@��w@���@�ff@�$�@�J@��@��#@��-@��7@�p�@�`B@�7L@��@�V@���@��/@��j@��@�z�@�1'@��
@��H@���@�v�@�V@�E�@�=q@��@���@�O�@�G�@�?}@�7L@�V@��@�(�@���@��@�l�@�;d@��@��H@��@���@�ȴ@��R@��!@���@�E�@�$�@��#@�7L@��@�V@�V@�%@���@��@��j@�bN@�(�@�@|�@;d@�@~�@~��@~v�@~V@~V@~V@~{@}�h@}?}@y�^@m?}@d�/@^ff@T�/@RJ@K��@D9X@?+@:^5@1�@+S�@&ff@#ƨ@V@%@��@%@
=q@	�@|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�^5A�`BA�`BA�dZA�bNA�bNA�ffA�bNA�ffA�hsA�jA�ffA�ffA�ffA�ffA�jA�ffA�ffA�ffA�ffA�hsA�jA�bNA�`BA�dZA�`BA�`BA�^5A�^5A�^5A�`BA�dZA�ffA�jA�l�A�l�A�n�A�x�A�x�A�v�A�v�A�z�A�|�A�|�A�~�A�~�A�|�A�~�A�~�A�x�A�`BA�bNA�dZA�p�A�M�A�VA���A��PA�ƨA���A���A�A��#A��7A�/A��A�M�A���A���A��A���A��DA�1A���A��#A���A�`BA��wA�;dA��A�%A�ĜA��A�bA�7LA���A��hA�A��A��A���A�oA�t�A�n�A�VA�5?A�=qA���A�;dA���A��mA��;A�33A�A�A�M�A�ffA��A�7A}�
A{�
Az1'Az5?Ay�Au��ArVApVAm�FAk�AjM�AiC�AhjAhbAg�-AgG�AfJAc�PAa�^A^�A\�uAZĜAY�AX=qAW+AV�9ATĜAR9XAQ\)AP��API�AO�AMl�AL �AK\)AJ�`AH�\AE33ACƨABĜA?ƨA<�A<��A<1'A;��A;�;A;ƨA;�A;��A;��A;��A;��A;G�A;?}A:�yA:��A:�A:�DA:jA:A�A:�A9�TA7O�A2^5A.�A-G�A,bNA,1A+��A+?}A++A*�A*^5A)��A)|�A)p�A)S�A)7LA)�A(��A(�HA(ĜA(��A(~�A(ffA(E�A(1'A(  A'|�A&��A&�9A&�DA&bA%K�A%"�A$�`A$�9A#��A"�A!+A��AA`BA �A�\A$�AXAI�A�;Al�A1A1A �A\)A/A�A��A+A$�A
=A
VA	�PA	XA	7LA	oA�A��A^5A�A��A�jA��A1'AbA�A��A��AO�A%A �R@��@��y@��u@�@�@�M�@�@�ƨ@@�;d@��@��@�Ĝ@땁@�+@�1@��H@�?}@�M�@��/@�I�@�1'@��
@�;d@�v�@�@�?}@�1'@�O�@���@�dZ@�G�@���@�bN@��@ύP@�ȴ@�J@�hs@̓u@�o@ț�@�\)@�=q@�I�@��H@���@���@��@�ȴ@�^5@��#@�Z@���@���@�J@�G�@��@�r�@���@��@��@��!@�5?@���@���@�r�@�ƨ@�\)@���@�n�@�E�@�{@��@��^@���@�hs@�%@�z�@���@��P@��@�M�@���@�p�@�%@�j@���@��@���@�v�@��-@��@�;d@�ff@��@�1'@�t�@�dZ@�C�@���@�M�@��@���@��^@���@�X@�&�@��@��9@�9X@��m@�l�@��y@�V@��T@�hs@�7L@��@���@�;d@�+@�+@�
=@�V@��h@�p�@�O�@��@���@��@�^5@���@��h@��7@�x�@�x�@�?}@��@��/@��@�bN@�b@�  @���@���@���@���@��@���@�ƨ@�+@�@��h@�p�@�hs@�X@�G�@�/@��@��j@��u@��@�j@�A�@�b@��w@���@�ff@�$�@�J@��@��#@��-@��7@�p�@�`B@�7L@��@�V@���@��/@��j@��@�z�@�1'@��
@��H@���@�v�@�V@�E�@�=q@��@���@�O�@�G�@�?}@�7L@�V@��@�(�@���@��@�l�@�;d@��@��H@��@���@�ȴ@��R@��!@���@�E�@�$�@��#@�7L@��@�V@�V@�%@���@��@��j@�bN@�(�@�@|�@;d@�@~�@~��@~v�@~V@~V@~V@~{@}�h@}?}@y�^@m?}@d�/@^ff@T�/@RJ@K��@D9X@?+@:^5@1�@+S�@&ff@#ƨ@V@%@��@%@
=q@	�@|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�yB�NB��B�-B=qB+B��B�mB�`B�HB�#BȴB��B�uB�B�Bu�B`BBXBW
BS�BN�BI�B=qB49B2-B0!B+B$�B�B
=B��B�B�5B�qB��B��B�BhsBS�B:^B�B
=BB
��B
�B
�/B
ɺB
�B
�B
e`B
W
B
J�B
=qB
0!B
 �B
�B
uB
1B	�B	�B	��B	�dB	�B	��B	��B	��B	��B	��B	��B	�PB	� B	t�B	hsB	_;B	S�B	J�B	D�B	>wB	<jB	8RB	(�B	%�B	$�B	�B	�B	\B	+B	B	B��B�B�`B�/B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺBɺBȴBȴBǮBƨBĜB��B�LB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�\B�VB�PB�DB�7B�%B�B}�Bw�Bq�Bm�BjBgmBffBcTBaHB`BB]/BYBS�BP�BO�BN�BM�BK�BH�BF�BE�BC�BB�BB�BA�BA�B@�B?}B>wB;dB7LB49B2-B1'B0!B0!B/B/B.B-B)�B%�B$�B �B�B�B�B�B�B�B�B�B�B{BuBoBoBhB\BbBbBbBbBbBbBbB\BVBPBPBPBbBoBoBoBoBoBoBoBoBoBoB{B�B�B�B�B�B�B�B!�B!�B!�B#�B&�B&�B'�B(�B(�B)�B+B-B-B,B,B2-B7LB;dB>wB>wBA�BB�BB�BC�BC�BD�BD�BE�BE�BG�BJ�BJ�BM�BO�BQ�BR�BS�BT�BXB[#B[#B[#B^5BffBhsBk�Bs�Bw�B{�B{�B|�B~�B�B�B�B�%B�%B�1B�7B�=B�DB�VB�bB�oB��B��B��B��B��B��B��B�B�B�B�B�-B�RB�RB�XB�^B�qBĜB��B��B��B��B��B��B��B�
B�B�#B�)B�5B�;B�;B�;B�;B�;B�;B�BB�BB�ZB�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	%B	DB	PB	VB	\B	bB	hB	oB	uB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	'�B	+B	,B	.B	.B	.B	/B	49B	7LB	8RB	8RB	8RB	9XB	<jB	A�B	F�B	G�B	G�B	I�B	J�B	M�B	M�B	N�B	N�B	O�B	O�B	O�B	S�B	T�B	W
B	]/B	_;B	_;B	_;B	_;B	`BB	`BB	bNB	e`B	hsB	jB	l�B	m�B	n�B	o�B	p�B	q�B	r�B	r�B	r�B	s�B	u�B	w�B	�1B	�9B	��B	�HB	��B	��B
	7B
�B
#�B
+B
8RB
?}B
E�B
H�B
P�B
W
B
\)B
aHB
k�B
l�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�pB�BB��B�"B=`BB��B�[B�OB�9B�BȢB��B�cB�B��Bu�B`0BW�BV�BS�BN�BI�B=aB4(B2B0B*�B$�B�B
)B��B�qB�#B�\B��B�pB�
BhaBS�B:LB�B
*B�B
��B
�uB
�B
ɬB
��B
��B
eQB
V�B
J�B
=bB
0B
 �B
rB
hB
&B	�B	�B	˾B	�ZB	�B	��B	��B	��B	��B	��B	�xB	�IB	�B	t�B	hkB	_3B	S�B	J�B	D�B	>nB	<dB	8KB	(�B	%�B	$�B	�B	�B	XB	'B	B	B��B�B�\B�,B��B��B��B��B��B��B��B��B��B��B��B��B��BʿBɷBɷBȳBȱBǪBƥBěB��B�IB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�zB�rB�oB�hB�\B�UB�OB�CB�9B�%B�B}�Bw�Bq�Bm�BjBglBfgBcVBaKB`CB]0BYBS�BP�BO�BN�BM�BK�BH�BF�BE�BC�BB�BB�BA�BA�B@�B?}B>vB;gB7MB47B2/B1*B0 B0$B/B/B.B-B)�B%�B$�B �B�B�B�B�B�B�B�B�BfB~BxBTBTBMB\BGBIBIBJBcBIBbB\B<B6BQBPBHBrBUBnBnBpBoBpBrBTBpBaB�B�BxB�B�B�B�B!�B!�B!�B#�B&�B&�B'�B(�B(�B)�B+B-B-B,B,B2)B7JB;cB>tB>vBA�BB�BB�BC�BC�BD�BD�BE�BE�BG�BJ�BJ�BM�BO�BQ�BR�BS�BT�BXB[B[B["B^2Bf`BhoBk�Bs�Bw�B{�B{�B|�B~�B�B�B�B�B�B�,B�3B�7B�=B�NB�\B�hB�yB��B��B��B��B��B��B�B�B�B�B�&B�JB�IB�NB�VB�jBĔB��B��B��B��B��B��B��B�B�B�B�B�-B�0B�0B�0B�1B�0B�0B�8B�8B�RB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B	8B	DB	MB	RB	TB	\B	`B	eB	jB	tB	xB	zB	�B	�B	�B	�B	�B	�B	�B	'�B	*�B	+�B	.B	.B	.B	/B	4*B	7>B	8CB	8DB	8DB	9IB	<\B	A{B	F�B	G�B	G�B	I�B	J�B	M�B	M�B	N�B	N�B	O�B	O�B	O�B	S�B	T�B	V�B	]B	_+B	_*B	_+B	_.B	`4B	`5B	b>B	eRB	hfB	joB	l|B	m�B	n�B	o�B	p�B	q�B	r�B	r�B	r�B	s�B	u�B	w�B	�!B	�(B	ʱB	�4B	��B	��B
	#B
�B
#�B
*�B
8>B
?fB
E�B
H�B
P�B
V�B
\B
a1B
kpB
lvB
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708162016053117081620160531170816  AO  ARCAADJP                                                                    20140721230840    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230840  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230840  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170816  IP                  G�O�G�O�G�O�                