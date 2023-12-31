CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-10T09:17:11Z AOML 3.0 creation; 2016-08-07T21:36:37Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20150710091711  20160807143637  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ;A   AO  5286_8897_059                   2C  D   APEX                            6531                            072314                          846 @�^��GP 1   @�^�}'�@1���v��cO\(�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ;A   B   B   @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DRfDR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D�	�D�0 D�� D��fD�3D�Y�D���D�� D�fD�<�D�s3D��3D�	�D�33D�y�D�� D�fD�L�D�D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @9��@�ff@�ffA33A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BY33B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffBș�B̙�B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR3DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj�gDk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt� Dy�gD� D�6fD��fD���D��D�` D�� D��fD��D�C3D�y�D�ٙD� D�9�Dڀ D��fD��D�S3D� D�ə1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1A�
=A�JA�VA�A���A��A��yA��A־wA֡�A�t�A�oAՑhA�&�A҉7A���A�S�A���A���AЙ�AЋDA�l�A�K�A�oA��A��A�ƨAσA��A�|�A�;dA��A�bA���A͇+A�E�A��A˲-A�oA�7LA�dZAǮA�G�AăA�"�A�C�A�=qA�VA��A�$�A�hsA���A���A�bA�VA���A��A���A�oA��A�oA�A��RA�1'A���A��^A�33A���A���A���A��A�v�A�S�A�bNA�&�A�A��uA�$�A�A��A���A���A���A�A��jA�?}A�S�A� �A�=qA���A��;A�A�I�A��RA�ZA��wA�A��#A��;A��A��A��-A�^5A��A���A{%Ax(�Au�Am`BAh�Af�jAe��Ac33Aa�#Aa/A`�yA_\)A[XAX�AV�AT��AQG�AO�FAN�`AM�hAK"�AI&�AE?}AC`BA?x�A=�A;�
A:�A9A81'A7��A6=qA4ffA4ZA4 �A2n�A1��A1%A/��A.I�A-�A,JA*ȴA&��A%|�A$��A"E�A!&�A��A�+A��A&�A��AhsAS�A
=A�^At�A��A�FAbA��A�AI�AXAI�A\)Az�A�wA�AO�A
�A��A�A;dAx�AoA �`A��A�yAAn�A��A�;An�A��A��AbNA$�A�A��AhsA+A%AȴA�
A��A{AƨA �@���@���@�o@���@��
@�
=@�=q@�K�@�;d@�Q�@��@�^@�J@�j@�P@�\@�o@��@�v�@�ȴ@�x�@�5?@�b@���@�33@�K�@�S�@�l�@�o@���@���@��@�=q@�dZ@�K�@��@���@�7L@܃@�;d@۝�@�A�@�p�@��@�C�@�Q�@ۮ@��@ڰ!@ڗ�@��y@�\)@ٙ�@�;d@�5?@֟�@�hs@Լj@��@�v�@���@�bN@�A�@Լj@ԣ�@ԓu@�I�@�S�@ҸR@�G�@�  @���@��@�  @Л�@мj@мj@У�@�(�@϶F@ϥ�@�l�@�o@·+@�5?@͉7@̴9@�A�@�j@˝�@���@��
@ǍP@�j@ȼj@Ǿw@ư!@�5?@��@�@Ƈ+@���@ũ�@��`@�=q@���@��@�j@���@�`B@��m@��R@�~�@��@��@�z�@��@�ƨ@�\)@�K�@�;d@���@���@��w@�|�@��@��y@���@�ff@�X@�Ĝ@�Q�@���@�l�@�o@��!@�n�@�5?@���@���@�x�@��@��/@�j@�9X@��;@�C�@��H@�ff@��@��T@��-@�p�@��@���@�(�@�A�@��@�dZ@�ȴ@�v�@�5?@���@��@��@��#@��h@�X@�V@���@��D@�1'@�ƨ@��F@��w@��@���@��P@�o@��R@��!@��@���@��+@�@��-@�O�@���@��j@�Q�@�1'@�1@���@���@�|�@�C�@��y@�=q@��@���@�V@��9@���@�Z@� �@�l�@��R@�{@���@�X@���@���@�(�@���@��m@��w@���@�l�@�+@��!@���@���@�?}@��@��@��@��@���@��@�bN@� �@��
@���@���@��!@���@�v�@�E�@�-@�-@�$�@��@�{@�@���@�p�@�?}@��@�Ĝ@��9@�Z@�(�@�1@��@�+@���@��@�@�x�@�`B@�X@�7L@���@�(�@��;@�l�@�33@�@���@���@�v�@�M�@���@�p�@��@��j@�Z@�1'@���@���@��@�S�@�
=@���@� �@�9X@��@�  @y&�@n�R@d�D@\�@T9X@K"�@C�@9��@5�T@.�R@)��@'l�@!�@��@��@�@�h1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�1A�
=A�JA�VA�A���A��A��yA��A־wA֡�A�t�A�oAՑhA�&�A҉7A���A�S�A���A���AЙ�AЋDA�l�A�K�A�oA��A��A�ƨAσA��A�|�A�;dA��A�bA���A͇+A�E�A��A˲-A�oA�7LA�dZAǮA�G�AăA�"�A�C�A�=qA�VA��A�$�A�hsA���A���A�bA�VA���A��A���A�oA��A�oA�A��RA�1'A���A��^A�33A���A���A���A��A�v�A�S�A�bNA�&�A�A��uA�$�A�A��A���A���A���A�A��jA�?}A�S�A� �A�=qA���A��;A�A�I�A��RA�ZA��wA�A��#A��;A��A��A��-A�^5A��A���A{%Ax(�Au�Am`BAh�Af�jAe��Ac33Aa�#Aa/A`�yA_\)A[XAX�AV�AT��AQG�AO�FAN�`AM�hAK"�AI&�AE?}AC`BA?x�A=�A;�
A:�A9A81'A7��A6=qA4ffA4ZA4 �A2n�A1��A1%A/��A.I�A-�A,JA*ȴA&��A%|�A$��A"E�A!&�A��A�+A��A&�A��AhsAS�A
=A�^At�A��A�FAbA��A�AI�AXAI�A\)Az�A�wA�AO�A
�A��A�A;dAx�AoA �`A��A�yAAn�A��A�;An�A��A��AbNA$�A�A��AhsA+A%AȴA�
A��A{AƨA �@���@���@�o@���@��
@�
=@�=q@�K�@�;d@�Q�@��@�^@�J@�j@�P@�\@�o@��@�v�@�ȴ@�x�@�5?@�b@���@�33@�K�@�S�@�l�@�o@���@���@��@�=q@�dZ@�K�@��@���@�7L@܃@�;d@۝�@�A�@�p�@��@�C�@�Q�@ۮ@��@ڰ!@ڗ�@��y@�\)@ٙ�@�;d@�5?@֟�@�hs@Լj@��@�v�@���@�bN@�A�@Լj@ԣ�@ԓu@�I�@�S�@ҸR@�G�@�  @���@��@�  @Л�@мj@мj@У�@�(�@϶F@ϥ�@�l�@�o@·+@�5?@͉7@̴9@�A�@�j@˝�@���@��
@ǍP@�j@ȼj@Ǿw@ư!@�5?@��@�@Ƈ+@���@ũ�@��`@�=q@���@��@�j@���@�`B@��m@��R@�~�@��@��@�z�@��@�ƨ@�\)@�K�@�;d@���@���@��w@�|�@��@��y@���@�ff@�X@�Ĝ@�Q�@���@�l�@�o@��!@�n�@�5?@���@���@�x�@��@��/@�j@�9X@��;@�C�@��H@�ff@��@��T@��-@�p�@��@���@�(�@�A�@��@�dZ@�ȴ@�v�@�5?@���@��@��@��#@��h@�X@�V@���@��D@�1'@�ƨ@��F@��w@��@���@��P@�o@��R@��!@��@���@��+@�@��-@�O�@���@��j@�Q�@�1'@�1@���@���@�|�@�C�@��y@�=q@��@���@�V@��9@���@�Z@� �@�l�@��R@�{@���@�X@���@���@�(�@���@��m@��w@���@�l�@�+@��!@���@���@�?}@��@��@��@��@���@��@�bN@� �@��
@���@���@��!@���@�v�@�E�@�-@�-@�$�@��@�{@�@���@�p�@�?}@��@�Ĝ@��9@�Z@�(�@�1@��@�+@���@��@�@�x�@�`B@�X@�7L@���@�(�@��;@�l�@�33@�@���@���@�v�@�M�@���@�p�@��@��j@�Z@�1'@���@���@��@�S�@�
=G�O�@� �@�9X@��@�  @y&�@n�R@d�D@\�@T9X@K"�@C�@9��@5�T@.�R@)��@'l�@!�@��@��@�@�h1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�DB
�DB
�=B
�DB
�DB
�DB
�JB
�JB
�JB
�PB
�PB
�\B
�{B
��B
�5BoB!�B-B49B33B33B33B6FB8RB;dB=qB>wB?}BH�BW
B|�B��BbB&�B:^BM�BZBk�Bv�B�oB��B�{B�+Bu�B�hB�B�yBoB"�B+B/B0!B6FB<jBN�BVBcTBffBhsBffBe`BaHB]/BP�BF�BA�B>wB5?B�BB��B��Bt�BQ�B'�B�B�/B��BȴB�3B��B�\B�hB��B�B�9B��B�+Bk�BL�BG�BW
BhsBYB:^B�B�B%B
�B
�#B
�dB
�B
YB
(�B
�B
B	�B	�}B	��B	q�B	O�B	D�B	<jB	2-B	-B	)�B	&�B	�B	VB	B��B��B�B�B�B�mB�BB�B��BǮB��B�jB�dB�^B�jB�wB��BŢB��B�B�B�B�B�B�B��B��B�B�ZBɺB�}B�dB�dB�dB�}B��B�wB��B�wB�jB�jB�wB�}BBĜBĜB�^B�FB�^BǮB��BƨBÖBȴBȴBɺB��BǮB�RB�B��B��B��B��B�!B��B�B�B��B	bB	�B	"�B	$�B	$�B	%�B	%�B	'�B	)�B	+B	)�B	(�B	&�B	#�B	!�B	�B	�B	hB	1B	B		7B	�B	#�B	 �B	�B	\B	DB	uB	�B	�B	�B	DB	%B	B	B	+B	VB	"�B	�B	�B	�B	PB	�B	�B	�B	�B	�B	�B	"�B	'�B	;dB	C�B	I�B	E�B	L�B	L�B	H�B	P�B	^5B	l�B	k�B	iyB	p�B	p�B	o�B	o�B	q�B	u�B	~�B	y�B	p�B	m�B	p�B	n�B	s�B	�+B	�+B	�B	�B	�B	�DB	�JB	�VB	�\B	�VB	�bB	�PB	�=B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�9B	�?B	�FB	�FB	�FB	�XB	�^B	�^B	�^B	�qB	�wB	�wB	�qB	�wB	��B	B	ĜB	ĜB	ĜB	ŢB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�)B	�5B	�5B	�;B	�HB	�NB	�ZB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
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
JB
1B
oB
�B
�B
!�B
'�B
,B
33B
:^B
A�B
F�B
M�B
XB
\)B
^5B
`BB
bNB
gmB
n�B
s�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B
�9B
�9B
�0B
�7B
�7B
�5B
�@B
�=B
�AB
�DB
�EB
�OB
�rB
��B
�%BaB!�B,�B4'B3#B3%B3$B69B8AB;SB=bB>eB?oBH�BV�B|�B��BPB&�B:PBM�BZBkvBv�B�bB��B�kB�Bu�B�WB��B�jBbB"�B*�B/B0B67B<_BN�BU�BcFBfWBhhBfZBeOBa;B]%BP�BF�BAwB>iB5/B�B �B˶B��Bt�BQ�B'�B�B�B��BȢB�#B��B�IB�TB��B�B�%B��B�BkqBL�BG�BV�BhaBYB:LB�BzBB
�B
�B
�UB
�B
YB
(�B
�B

B	�B	�sB	��B	q�B	O�B	D�B	<eB	2'B	-B	)�B	&�B	�B	NB	B��B��B�B�B�B�jB�>B�B��BǪB��B�hB�aB�\B�hB�tB��BŞB��B�B�B�B�B�~B�B��B��B�B�TBɵB�xB�aB�`B�_B�xB�~B�rB�|B�qB�cB�fB�pB�yBBĘBĘB�[B�BB�ZBǧBʼBƤBÏBȰBȯBɴB��BǥB�LB�B��B��B��B��B�B��B��B�B��B	[B	�B	"�B	$�B	$�B	%�B	%�B	'�B	)�B	*�B	)�B	(�B	&�B	#�B	!�B	�B	�B	_B	'B	B		,B	�B	#�B	 �B	�B	RB	;B	kB	�B	�B	�B	8B	B	B	B	"B	KB	"�B	�B	�B	wB	EB	�B	�B	�B	�B	�B	�B	"�B	'�B	;XB	C�B	I�B	E�B	L�B	L�B	H�B	P�B	^(B	l~B	kxB	ikB	p�B	p�B	o�B	o�B	q�B	u�B	~�B	y�B	p�B	m�B	p�B	n�B	s�B	�B	�B	�B	��B	�B	�5B	�;B	�GB	�MB	�HB	�VB	�AB	�/B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�)B	�.B	�3B	�5B	�3B	�FB	�MB	�MB	�PB	�_B	�gB	�fB	�_B	�gB	�xB	�~B	ĊB	ĊB	ĊB	ŏB	ƘB	ǜB	ǚB	ɨB	˴B	˵B	̻B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�$B	�"B	�(B	�5B	�;B	�IB	�SB	�ZB	�bB	�fB	�jB	�jB	�kB	�jB	�kB	�nB	�kB	�lB	�tB	�yB	�B	�B	�B	�B	�B	�}B	�}B	�B	�B	�}B	�B	�B	�B	�B	�B	�B	�~B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
B
�B
�B
 B
 B
B
B

B
B
B
B
B
G�O�B
B
XB
�B
�B
!�B
'�B
+�B
3B
:JB
AtB
F�B
M�B
W�B
\B
^!B
`,B
b8B
gWB
n�B
s�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436372016080714363720160807143637  AO  ARCAADJP                                                                    20150710091711    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150710091711  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150710091711  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143637  IP                  G�O�G�O�G�O�                