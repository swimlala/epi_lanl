CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:28Z AOML 3.0 creation; 2016-08-07T21:51:13Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221428  20160807145113  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_026                   2C  D   APEX                            6529                            072314                          846 @�.nlw� 1   @�.o @2��R�db��`A�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�CffC��C	�fCffC�C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�DyY�D�	�D�,�D��3D��3D��D�@ D�|�D��fD��D�33D�i�D��fD�3D�L�Dڐ D�fD� D�FfD�p D�s3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @HQ�@�\)@�\)A�A#�AC�Ac�A��
A��
A��
A��
A��
A��
A��
A��
B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�C :�C:�CT{C�GC�C
!GC�GCT{C!GC:�C:�C:�C:�C:�C:�C:�C :�C":�C$:�C&:�C(:�C*:�C,:�C.:�C0T{C2:�C4:�C6:�C8:�C::�C<:�C>:�C@:�CB:�CD:�CF:�CH:�CJ:�CL:�CN:�CP:�CR:�CT:�CV:�CX:�CZ:�C\:�C^:�C`:�Cb:�Cd:�CfT{Ch:�Cj:�Cl:�Cn:�Cp:�Cr:�Ct:�Cv:�Cx:�Cz:�C|:�C~:�C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt{�DyhRD��D�4)D���D���D� �D�G\D��)D���D�)D�:�D�p�D���D��D�T)Dڗ\D��D�\D�M�D�w\D�z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AξwAζFAδ9AζFAθRA���AξwAξwAθRA�A���A���A���A���A���A���A���A���A���A���A���A��#A��
A��
A���A���A��A���A��
A��#A��;A��;A��TA��TA��`A��`A��mA��mA��mA��yA��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��yA��A��A��A���A���A���A���A���A���Aΰ!AΥ�A̓A��`A�9XA�  A�ZA�+A�A�A��TA���A�n�A���A�9XA��A�9XA�\)A�9XA��A��A�ĜA��A�M�A�Q�A�p�A���A��A��A�hsA���A��A��`A�I�A��hA��^A���A�r�A�`BA�%A��+A���A�-A�ffA���A��^A�=qA��RA�r�A��A�5?A�A~�+A{��AxQ�Ap1Ajn�Ad��A\ZAT�!APbNAL�AKx�AI��AHv�AE�#ADĜAC�wAA�A@ZA?C�A> �A;hsA:-A9�;A9��A8�A6�uA5�7A4bA2A�A.v�A-�-A-/A,�uA+\)A*v�A)K�A(bA'hsA&jA$�RA#l�A"1AC�An�A��A7LA�A�7A�A1'A�!AE�A��A�7A7LA��A^5A�TA�A��A�A��A��A=qA+AVAZA��A�A��AXAoA�A�AZAJA��A`BA�yA$�A	�A	�A	��A	"�A�HA�jAz�A�A/AffAp�A�RA�A;dA�A�A�
Ap�A ��A 9X@�M�@�1@���@��7@��D@�1@��@�$�@��T@��@���@�ȴ@�@�@��@�j@�@�l�@�9X@�dZ@�^5@�@��^@��/@�@���@�@�9@�1@�P@���@�M�@�@�h@��@�u@�C�@�-@�@���@��u@��;@�33@��H@޸R@ޏ\@�{@ݡ�@�O�@�V@��`@�Z@�Z@�bN@�Q�@��@�|�@ڸR@�=q@�@���@�{@٩�@�&�@��/@ش9@�Z@�1@ו�@�@ְ!@�ff@�E�@�$�@��T@�x�@�/@���@Ԭ@�I�@���@Ӆ@�l�@�@���@ҟ�@�~�@�V@�5?@�J@Ѳ-@�X@��@���@Ь@�(�@�|�@�
=@�ȴ@·+@�E�@��@ͺ^@�x�@�X@�&�@�%@���@��/@��;@�K�@�M�@��T@ɩ�@ɑh@�p�@�&�@�z�@�(�@�ƨ@�S�@��@��@Ɨ�@���@�O�@ļj@�z�@�A�@�t�@��@�$�@���@�I�@���@���@���@�C�@��!@�^5@�{@��-@�/@���@�z�@��m@�\)@��@��H@�=q@��-@�p�@�?}@�V@�Q�@���@��@��+@�V@��@���@�V@���@��D@��m@��@���@�~�@�5?@���@���@���@�Q�@��@�l�@��!@�v�@��^@���@��@�;d@�t�@�S�@��H@���@�J@�p�@���@��@��D@�j@�b@�\)@�+@��@���@�$�@�@��@��@�Z@�A�@��;@���@��!@�~�@�ff@�E�@��-@�x�@�x�@�p�@�`B@�V@��/@��D@�1'@�b@��@�;d@�@��@�ȴ@��\@�ff@�=q@�J@���@���@���@�p�@�?}@���@�I�@�(�@�b@��@���@��F@���@�K�@�ȴ@�^5@�J@���@��@���@���@�G�@���@���@�Z@�1'@��;@��w@���@��@�K�@�
=@�n�@���@���@��@�O�@�7L@��/@���@�I�@��@���@��F@���@��@�@�bN@��@{@rM�@iG�@`  @W��@S��@Kƨ@Dz�@=@6ȴ@1�@*��@"�\@��@��@ȴ@"�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AξwAζFAδ9AζFAθRA���AξwAξwAθRA�A���A���A���A���A���A���A���A���A���A���A���A��#A��
A��
A���A���A��A���A��
A��#A��;A��;A��TA��TA��`A��`A��mA��mA��mA��yA��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��yA��A��A��A���A���A���A���A���A���Aΰ!AΥ�A̓A��`A�9XA�  A�ZA�+A�A�A��TA���A�n�A���A�9XA��A�9XA�\)A�9XA��A��A�ĜA��A�M�A�Q�A�p�A���A��A��A�hsA���A��A��`A�I�A��hA��^A���A�r�A�`BA�%A��+A���A�-A�ffA���A��^A�=qA��RA�r�A��A�5?A�A~�+A{��AxQ�Ap1Ajn�Ad��A\ZAT�!APbNAL�AKx�AI��AHv�AE�#ADĜAC�wAA�A@ZA?C�A> �A;hsA:-A9�;A9��A8�A6�uA5�7A4bA2A�A.v�A-�-A-/A,�uA+\)A*v�A)K�A(bA'hsA&jA$�RA#l�A"1AC�An�A��A7LA�A�7A�A1'A�!AE�A��A�7A7LA��A^5A�TA�A��A�A��A��A=qA+AVAZA��A�A��AXAoA�A�AZAJA��A`BA�yA$�A	�A	�A	��A	"�A�HA�jAz�A�A/AffAp�A�RA�A;dA�A�A�
Ap�A ��A 9X@�M�@�1@���@��7@��D@�1@��@�$�@��T@��@���@�ȴ@�@�@��@�j@�@�l�@�9X@�dZ@�^5@�@��^@��/@�@���@�@�9@�1@�P@���@�M�@�@�h@��@�u@�C�@�-@�@���@��u@��;@�33@��H@޸R@ޏ\@�{@ݡ�@�O�@�V@��`@�Z@�Z@�bN@�Q�@��@�|�@ڸR@�=q@�@���@�{@٩�@�&�@��/@ش9@�Z@�1@ו�@�@ְ!@�ff@�E�@�$�@��T@�x�@�/@���@Ԭ@�I�@���@Ӆ@�l�@�@���@ҟ�@�~�@�V@�5?@�J@Ѳ-@�X@��@���@Ь@�(�@�|�@�
=@�ȴ@·+@�E�@��@ͺ^@�x�@�X@�&�@�%@���@��/@��;@�K�@�M�@��T@ɩ�@ɑh@�p�@�&�@�z�@�(�@�ƨ@�S�@��@��@Ɨ�@���@�O�@ļj@�z�@�A�@�t�@��@�$�@���@�I�@���@���@���@�C�@��!@�^5@�{@��-@�/@���@�z�@��m@�\)@��@��H@�=q@��-@�p�@�?}@�V@�Q�@���@��@��+@�V@��@���@�V@���@��D@��m@��@���@�~�@�5?@���@���@���@�Q�@��@�l�@��!@�v�@��^@���@��@�;d@�t�@�S�@��H@���@�J@�p�@���@��@��D@�j@�b@�\)@�+@��@���@�$�@�@��@��@�Z@�A�@��;@���@��!@�~�@�ff@�E�@��-@�x�@�x�@�p�@�`B@�V@��/@��D@�1'@�b@��@�;d@�@��@�ȴ@��\@�ff@�=q@�J@���@���@���@�p�@�?}@���@�I�@�(�@�b@��@���@��F@���@�K�@�ȴ@�^5@�J@���@��@���@���@�G�@���@���@�Z@�1'@��;@��w@���@��@�K�@�
=@�n�@���@���@��@�O�@�7L@��/@���@�I�@��@���@��FG�O�@��@�@�bN@��@{@rM�@iG�@`  @W��@S��@Kƨ@Dz�@=@6ȴ@1�@*��@"�\@��@��@ȴ@"�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�5B"�B&�B?}Be`Bq�By�B{�B{�By�B|�B~�B{�Bx�Bs�BiyBffBcTB`BBXBI�B9XB1'B-B�BJBB�B�
B��B��B��B��B�%Bq�B_;BA�B&�B{B1B
��B
��B
�VB
s�B
YB
-B
�B
�B
�B
	7B	��B	�B	��B	t�B	N�B	�B��B�mB�;B�B�
B��B��B��B��B��BȴBǮBɺB��B��BɺBȴBǮBǮBǮBƨBƨBɺB��B��BɺB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�#B�)B�BB�B�B��B��B��B�B�B��B��B��B��B��B	B	DB	VB	VB	oB	�B	�B	"�B	%�B	+B	.B	-B	.B	49B	;dB	<jB	?}B	@�B	C�B	E�B	E�B	M�B	Q�B	XB	ZB	^5B	gmB	jB	l�B	l�B	m�B	l�B	k�B	hsB	k�B	k�B	gmB	e`B	cTB	_;B	\)B	YB	W
B	W
B	W
B	ZB	\)B	^5B	_;B	`BB	`BB	bNB	bNB	bNB	e`B	iyB	p�B	q�B	q�B	q�B	z�B	~�B	�B	�B	�B	�B	�B	�B	�B	�+B	�=B	�7B	�DB	�JB	�bB	�bB	�hB	�hB	�oB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�-B	�3B	�9B	�?B	�LB	�RB	�RB	�XB	�XB	�XB	�^B	�dB	�jB	�qB	�}B	�}B	��B	ÖB	ĜB	ŢB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�)B	�)B	�/B	�/B	�5B	�5B	�;B	�;B	�BB	�BB	�BB	�BB	�HB	�BB	�BB	�HB	�HB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�TB	�TB	�NB	�TB	�TB	�TB	�ZB	�`B	�mB	�sB	�sB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�mB	�sB	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
1B
	7B
DB
hB
�B
�B
!�B
&�B
,B
5?B
@�B
A�B
E�B
I�B
O�B
ZB
_;B
dZB
iyB
m�B
q�B
t�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B˳B˳B˵B˳B˵B˵B˵B˵B˳B˳B˶BʭBʫB˶B˳B˳BʭBʭBʭBʰBʭB˵BʫBʭBʭB˷BʭB˶B˳B˵B˳B˵B˶B˶BʭBʰBʭBʭBʰBʭBʭBʭBʭBʭBʭBʰBʭBʭB˳BʭBʭBʰB˶B˵B˳B˶B˵B̺B̼B̼B��B��B��B��B��B��B��B��B�&B"�B&�B?kBeKBq�By�B{�B{�By�B|�B~�B{�Bx�Bs�BidBfSBc=B`/BW�BI�B9AB1B,�BwB2B�B�jB��BͽB�nB��B�lB�Bq�B_&BAtB&�BiBB
��B
�sB
�DB
s�B
YB
,�B
�B
�B
oB
	&B	��B	��B	��B	t�B	N�B	�B��B�gB�6B�B�B��B��B��B��B��BȮBǧBɵBʼBʺBɳBȮBǧBǧBǥBƞBƞBɲBʺBʺBɴB˾B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�6B�B�B��B��B��B�B�B��B��B��B��B��B	B	8B	KB	JB	cB	uB	�B	"�B	%�B	*�B	.B	-B	.B	4+B	;UB	<\B	?pB	@wB	C�B	E�B	E�B	M�B	Q�B	XB	ZB	^#B	g]B	jnB	l~B	l{B	m�B	l}B	ktB	hdB	ktB	kvB	g^B	ePB	cDB	_+B	\B	YB	V�B	V�B	V�B	ZB	\B	^#B	_,B	`1B	`1B	b<B	bAB	b<B	ePB	ihB	p�B	q�B	q�B	q�B	z�B	~�B	�B	�	B	�B	� B	��B	�B	�B	�B	�+B	�&B	�2B	�8B	�PB	�RB	�TB	�SB	�]B	�^B	�aB	�kB	�jB	�lB	�nB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	�B	�%B	�,B	�8B	�>B	�=B	�CB	�CB	�EB	�LB	�QB	�VB	�\B	�gB	�hB	�wB	ÀB	ĈB	ŎB	ƔB	ƕB	ǛB	ɦB	˲B	˲B	̸B	ͽB	��B	��B	��B	��B	��B	�B	��B	�B	� B	�B	�B	�B	�B	�B	�B	�B	� B	�%B	�%B	�+B	�,B	�+B	�+B	�5B	�.B	�*B	�1B	�2B	�8B	�=B	�@B	�DB	�BB	�CB	�=B	�?B	�6B	�<B	�=B	�=B	�CB	�JB	�VB	�^B	�]B	�UB	�XB	�]B	�\B	�dB	�gB	�gB	�gB	�iB	�qB	�oB	�jB	�nB	�oB	�nB	�oB	�gB	�hB	�mB	�qB	�nB	�nB	�gB	�pB	�pB	�gB	�\B	�VB	�VB	�iB	�jB	�gB	�oB	�nB	�hB	�iB	�iB	�jB	�bB	�\B	�UB	�[B	�bB	�\B	�^B	�dB	�iB	�tB	�vB	�tB	�zB	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
B
B
B
B
B
B
B
 B
B
B
B
B
B
B
B
B
B
G�O�B
-B
OB
�B
�B
!�B
&�B
+�B
5(B
@hB
ApB
E�B
I�B
O�B
ZB
_!B
d@B
i`B
mwB
q�B
t�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.23 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451132016080714511320160807145113  AO  ARCAADJP                                                                    20150226221428    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221428  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221428  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145113  IP                  G�O�G�O�G�O�                