CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:56Z AOML 3.0 creation; 2016-08-07T21:36:32Z UW 3.1 conversion     
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
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221356  20160807143632  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               !A   AO  5286_8897_033                   2C  D   APEX                            6531                            072314                          846 @�<�=p��1   @�<����@2���R�c�p��
=1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    !A   B   B   @   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD� D�I�D���D��3D�3D�FfD�i�D���D��3D�6fD��3D�ɚD���D�@ Dڀ D���D�  D�FfD�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @.�R@�\)@�\)A�A#�AC�Ac�A��
A��
A��
A��
A��
A��
A��
A��B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�C :�C:�C:�C:�C:�C
:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C :�C":�C$:�C&T{C(:�C*:�C,:�C.:�C0:�C2:�C4:�C6:�C8:�C::�C<:�C>:�C@:�CB:�CD:�CF:�CH:�CJ:�CL:�CN:�CP:�CR:�CT:�CV:�CX:�CZ:�C\T{C^:�C`:�Cb:�Cd:�Cf:�Ch:�Cj:�Cl:�Cn:�Cp:�Cr:�Ct:�Cv:�Cx:�Cz:�C|:�C~:�C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�RDy�D�\D�P�D���D���D��D�M�D�p�D��)D���D�=�D���D���D�)D�G\Dڇ\D��)D�\D�M�D��D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�O�A�$�A�A���A͑hA�r�A�bNA�^5A�S�A�\)A�bNA�A�A�{A��A�  A�VA��;A���A��
A��A���A�p�A�5?A��`A˟�A�ZA�(�A�(�A�"�A��A�%A���A���A�ȴA�ȴA�E�Aɧ�A�+A�~�A��;A��A�C�AŶFAœuA�x�A��
AA��A�bA���A�  A��HA�ĜA�~�A�dZA��A��yA�I�A���A�ƨA�"�A�r�A�33A�A���A�ƨA���A���A�|�A��\A��RA�\)A�A�I�A��FA�hsA�oA��uA�|�A���A�S�A�9XA�M�A��A���A��jA��PA�~�A��wA�S�A���A���A��TA���A�C�A~��Az��Ayp�Ay
=Ax �Au�TAt�`As��Aq�-An9XAk��Ah��Af�yAb�A]`BAZn�AYt�AV1AN��AJZAF�DAE��AE��ACt�A?�A:�HA8r�A5l�A3|�A2�A1�A/��A/oA.�DA-�-A,bNA+�7A*�RA)VA(�+A(VA'�
A'�7A'O�A'+A&5?A$VA"��A"9XA!�TA!G�A �AA�/A��AE�A�AE�A�A �A+A�9A��Ax�A"�A�\AA�A�Ap�A
=A�9Ar�Ar�A��A�mAz�A1A��A;dA�A��AjA�A�^Ap�A
�yA
jA
(�A	`BA  A�`AM�AbA�#A��A�A/A�/A �A�
A|�A�AK�A �HA �R@���@�v�@�hs@���@�o@���@�t�@�;d@��\@�J@���@�l�@���@�r�@�"�@홚@�$�@��@��@��#@���@�=q@��/@�A�@�  @�K�@�x�@�@�(�@�  @��
@ߕ�@߅@ޗ�@���@�A�@�dZ@�dZ@�33@�ȴ@ڇ+@��@և+@���@��@�ff@щ7@�bN@�\)@ΰ!@�5?@�$�@�-@��@��@���@˾w@�\)@���@�$�@���@ɑh@�G�@��@��T@ɡ�@ɺ^@��@�J@���@�@ɺ^@��y@���@ʗ�@ʸR@ʗ�@ʏ\@�~�@�^5@�E�@��@�x�@���@��`@���@���@���@���@�Ĝ@ȓu@��;@Ɵ�@�ff@���@�/@�I�@ÍP@�o@�=q@��#@�@�7L@�Ĝ@�A�@� �@��@��@��@��m@�
=@��\@�-@��^@��@��`@�Q�@�dZ@���@���@�n�@���@��h@�?}@���@�j@�1@��F@�o@�^5@��@��7@�hs@��h@�x�@��@��j@�I�@���@�C�@��y@�~�@�^5@�hs@���@�b@�|�@�+@���@��\@�^5@�-@��#@���@�x�@�X@�/@��/@���@�bN@�A�@��@�K�@�o@��@�ȴ@�n�@�@��@��@��/@�j@�  @���@�l�@�S�@�33@��@�~�@�$�@���@�p�@���@��`@�&�@��@��@�Q�@�b@�;d@���@�v�@��@���@���@���@�x�@��j@�z�@�bN@�bN@�(�@�  @��F@�dZ@�33@�o@�@��y@��!@�n�@��#@���@�`B@�bN@�ƨ@��@���@��\@��+@�~�@�v�@�n�@�^5@�V@�5?@��T@��7@�/@���@�9X@���@��P@��P@�
=@�{@��T@��@��@�x�@�?}@���@��@��/@��/@��/@��9@�Z@�I�@�(�@�b@�1@�  @��;@���@�C�@�o@�@���@���@��\@��+@�~�@�=q@��@�p�@�O�@�V@��@�r�@�1'@�  @��;@���@��F@�ƨ@�dZ@��H@��!@�^5@���@��-@��h@�X@�?}@�/@�&�@��@��j@�-@���@�r�@xr�@pA�@e��@]��@V�R@P �@J=q@B�H@;��@7��@2�@,1@$�@!�@?}@�^@Z@Q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�O�A�$�A�A���A͑hA�r�A�bNA�^5A�S�A�\)A�bNA�A�A�{A��A�  A�VA��;A���A��
A��A���A�p�A�5?A��`A˟�A�ZA�(�A�(�A�"�A��A�%A���A���A�ȴA�ȴA�E�Aɧ�A�+A�~�A��;A��A�C�AŶFAœuA�x�A��
AA��A�bA���A�  A��HA�ĜA�~�A�dZA��A��yA�I�A���A�ƨA�"�A�r�A�33A�A���A�ƨA���A���A�|�A��\A��RA�\)A�A�I�A��FA�hsA�oA��uA�|�A���A�S�A�9XA�M�A��A���A��jA��PA�~�A��wA�S�A���A���A��TA���A�C�A~��Az��Ayp�Ay
=Ax �Au�TAt�`As��Aq�-An9XAk��Ah��Af�yAb�A]`BAZn�AYt�AV1AN��AJZAF�DAE��AE��ACt�A?�A:�HA8r�A5l�A3|�A2�A1�A/��A/oA.�DA-�-A,bNA+�7A*�RA)VA(�+A(VA'�
A'�7A'O�A'+A&5?A$VA"��A"9XA!�TA!G�A �AA�/A��AE�A�AE�A�A �A+A�9A��Ax�A"�A�\AA�A�Ap�A
=A�9Ar�Ar�A��A�mAz�A1A��A;dA�A��AjA�A�^Ap�A
�yA
jA
(�A	`BA  A�`AM�AbA�#A��A�A/A�/A �A�
A|�A�AK�A �HA �R@���@�v�@�hs@���@�o@���@�t�@�;d@��\@�J@���@�l�@���@�r�@�"�@홚@�$�@��@��@��#@���@�=q@��/@�A�@�  @�K�@�x�@�@�(�@�  @��
@ߕ�@߅@ޗ�@���@�A�@�dZ@�dZ@�33@�ȴ@ڇ+@��@և+@���@��@�ff@щ7@�bN@�\)@ΰ!@�5?@�$�@�-@��@��@���@˾w@�\)@���@�$�@���@ɑh@�G�@��@��T@ɡ�@ɺ^@��@�J@���@�@ɺ^@��y@���@ʗ�@ʸR@ʗ�@ʏ\@�~�@�^5@�E�@��@�x�@���@��`@���@���@���@���@�Ĝ@ȓu@��;@Ɵ�@�ff@���@�/@�I�@ÍP@�o@�=q@��#@�@�7L@�Ĝ@�A�@� �@��@��@��@��m@�
=@��\@�-@��^@��@��`@�Q�@�dZ@���@���@�n�@���@��h@�?}@���@�j@�1@��F@�o@�^5@��@��7@�hs@��h@�x�@��@��j@�I�@���@�C�@��y@�~�@�^5@�hs@���@�b@�|�@�+@���@��\@�^5@�-@��#@���@�x�@�X@�/@��/@���@�bN@�A�@��@�K�@�o@��@�ȴ@�n�@�@��@��@��/@�j@�  @���@�l�@�S�@�33@��@�~�@�$�@���@�p�@���@��`@�&�@��@��@�Q�@�b@�;d@���@�v�@��@���@���@���@�x�@��j@�z�@�bN@�bN@�(�@�  @��F@�dZ@�33@�o@�@��y@��!@�n�@��#@���@�`B@�bN@�ƨ@��@���@��\@��+@�~�@�v�@�n�@�^5@�V@�5?@��T@��7@�/@���@�9X@���@��P@��P@�
=@�{@��T@��@��@�x�@�?}@���@��@��/@��/@��/@��9@�Z@�I�@�(�@�b@�1@�  @��;@���@�C�@�o@�@���@���@��\@��+@�~�@�=q@��@�p�@�O�@�V@��@�r�@�1'@�  @��;@���@��F@�ƨ@�dZ@��H@��!@�^5@���@��-@��h@�X@�?}@�/@�&�@��G�O�@�-@���@�r�@xr�@pA�@e��@]��@V�R@P �@J=q@B�H@;��@7��@2�@,1@$�@!�@?}@�^@Z@Q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�jB
�jB
�dB
�^B
�^B
�XB
�XB
�XB
�^B
ƨB
�;B
�NB
�sB
�B  B6FB� B�oB��B�-B��B�sB��BJB�B�B0!B;dB=qB>wBE�BVBXBZB[#B�B�XBȴB�#B�ZB�B��B  B  B��B�B�BB��B� BbNBe`Bz�B�B�7B�JB�bB�bB�bB�PB�hB�VB�PB�VB�hB�hB�oB�oB�PB^5B-B!�BbBB�BǮB��B|�Bl�BgmBk�BVB(�B
��B
�5B
�B
��B
B
�LB
��B
}�B
Q�B
@�B
$�B
	7B	�B	��B	�^B	�FB	�!B	��B	��B	�uB	�B	q�B	`BB	P�B	B�B	,B	{B	+B��B�B��BÖB�}B�jB�XB�9B�'B�B��B�B�'B�-B�'B�-B�-B�-B�-B�-B�-B�-B�3B�'B�'B�'B�'B�'B�!B�!B�3B�FB�LB�LB�RB�RB�dB�qB�qB�jB�qB�qB�dB�RB�dB�^B�jB�qB�qB�qB�qB�qB�qB�qB�qB�qB�dB�jB�}B��B��B��B��B��BBĜBƨBǮBǮBȴBƨBÖB��BBĜBĜBĜBŢBĜBĜBƨBɺB��B��B��BȴBƨBƨBƨBĜBBBB��BĜBĜBÖBB��B��B��BBÖBȴB��B�)B�5B�#B�B�B�)B�)B�)B�)B�)B�;B�mB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B	B	%B	+B	
=B	JB	PB	VB	VB	uB	�B	�B	�B	#�B	'�B	(�B	.B	:^B	A�B	H�B	P�B	W
B	ZB	ZB	[#B	]/B	l�B	l�B	z�B	�B	�B	�B	�B	�%B	�JB	�VB	�bB	�oB	�oB	�uB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�'B	�3B	�9B	�9B	�9B	�3B	�3B	�?B	�FB	�?B	�9B	�9B	�?B	�XB	�^B	�RB	�RB	�dB	�}B	B	��B	ÖB	ÖB	B	��B	��B	��B	B	B	B	ÖB	ÖB	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�/B	�)B	�/B	�/B	�/B	�/B	�5B	�;B	�;B	�BB	�;B	�BB	�HB	�`B	�mB	�fB	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�`B	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
  B
  B
  B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
  B
  B
  B
  B
  B
  B
B
B
B
B
%B
+B
1B
	7B
	7B
	7B

=B

=B
DB
DB
DB
DB

=B
JB
JB
oB
�B
�B
#�B
(�B
2-B
8RB
<jB
A�B
G�B
L�B
P�B
W
B
ZB
`BB
cTB
ffB
iyB
n�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�YB
�ZB
�VB
�NB
�QB
�JB
�JB
�HB
�NB
ƙB
�-B
�=B
�bB
�B
��B64B�B�\B��B�B�xB�`B��B7BtB�B0B;RB=bB>dBE�BU�BW�BZB[B�B�GBȣB�B�IB�B��B��B��B��B�oB�B�B��B�Bb9BeKBz�B��B� B�8B�PB�OB�OB�<B�VB�@B�<B�DB�WB�SB�_B�]B�>B^"B,�B!�BKB �B�BǜB��B|�BlyBg[BkrBU�B(�B
��B
�B
�B
��B
�wB
�9B
��B
}�B
Q�B
@rB
$�B
	(B	�B	�}B	�RB	�8B	�B	��B	��B	�hB	�B	q�B	`8B	P�B	B�B	+�B	tB	#B��B�B��BÐB�yB�dB�RB�6B�B�	B��B�B�!B�%B� B�&B�&B�$B�)B�&B�&B�&B�+B�"B� B� B�"B�!B�B�B�*B�?B�CB�DB�IB�KB�\B�fB�hB�bB�iB�kB�\B�IB�]B�XB�bB�jB�hB�gB�jB�hB�jB�iB�fB�gB�ZB�aB�uB��B��B��B��B�zBBĔBƟBǡBǣBȬBƞBÎB��BBĔBēBēBřBĔBēBƝBɯB˼B˽BʷBȪBƝBƝBƚBēBBBB�BĔBĕBÎBB�B�xB�zBBËBȦB��B�B�*B�B�B�B�B�B�B�B�B�/B�aB�yB�uB�yB�{B�xB�B�B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	B	
.B	;B	DB	HB	EB	gB	�B	�B	�B	#�B	'�B	(�B	.B	:OB	AxB	H�B	P�B	V�B	ZB	ZB	[B	]B	lyB	lyB	z�B	��B	�B	�B	�B	�B	�5B	�CB	�QB	�YB	�ZB	�aB	�bB	�hB	�hB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�$B	�"B	�#B	�B	�B	�*B	�1B	�*B	�#B	�%B	�+B	�BB	�JB	�@B	�=B	�RB	�gB	�yB	�tB	ÀB	�B	�}B	�sB	�oB	�oB	�yB	�xB	�|B	ÁB	ÁB	ĈB	ŌB	ƓB	ǚB	ȞB	ɤB	ɥB	ɣB	ʫB	ʬB	˰B	̷B	ͻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�%B	�,B	�%B	�,B	�2B	�IB	�WB	�RB	�HB	�JB	�HB	�IB	�IB	�PB	�OB	�HB	�PB	�^B	�cB	�hB	�nB	�nB	�wB	�zB	�{B	�|B	�{B	�zB	�{B	�|B	�zB	�zB	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
B
B
B
B
	B
	 B
	B

$B

"B
-B
-B
+B
-B

&G�O�B
4B
XB
wB
�B
#�B
(�B
2B
89B
<OB
AqB
G�B
L�B
P�B
V�B
ZB
`(B
c:B
fOB
i`B
n~B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.23 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436332016080714363320160807143633  AO  ARCAADJP                                                                    20150226221356    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221356  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221356  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143633  IP                  G�O�G�O�G�O�                