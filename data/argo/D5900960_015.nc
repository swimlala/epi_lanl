CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  3   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-01T23:26:49Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7T   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    88   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8<   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8D   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8H   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8P   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8X   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8`   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8d   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8l   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9l   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9x   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9|   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  >H   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ?|   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  DH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  E|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  JH   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  O   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  PH   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  U   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  VH   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  [   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  _�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  e�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  g   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  k�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    l   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    o   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    r   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  u   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    u<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    u@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    uD   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    uH   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  uL   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    u�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    u�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    u�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         u�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         u�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        u�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    u�Argo profile    3.1 1.2 19500101000000  5900960 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130103316  20190523124443  1514_5041_015                   2C  D   APEX                            2041                            062805                          846 @��ӛ 1   @��ӛ @6�t�j~��c#�z�H1   GPS     Primary sampling: mixed [deeper than nominal 1000dbar: discrete; nominal 1000dbar to surface: 2dbar-bin averaged]                                                                                                                                                  A   A   A   C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� DlٚDy@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 C��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dl��Dy331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A@�\A?�A>�+A=�A<9XA<A;�wA:�/A:ZA9�hA8�A7��A7�A6�RA4�/A3\)A1A1&�A0ZA.�!A.M�A-
=A+�mA*jA*�A(�HA'�A'K�A&�jA&�uA$�`A"-A!33A|�A��AS�Az�A��AC�A��A�A|�A�RA�#AoA��A�AoA��AVAS�A-AK�A
=AM�AƨAoA
$�A	��A�!A1'A`BA��A�Az�A�;Ax�A��A �+@��P@�V@��@��P@���@��u@�M�@��@�l�@���@�  @���@��;@�=q@�&�@�+@�1'@��@�@ݙ�@�I�@�t�@�+@ف@��m@�@Ӿw@ѡ�@υ@Ͳ-@�1@˕�@�;d@���@�r�@��
@Ǯ@��@��@ēu@§�@�Z@���@�I�@� �@�b@��;@�|�@�v�@�@�p�@��@�1@��y@�J@���@�33@�J@��@��@�1@�C�@�~�@�$�@��`@�bN@�9X@��@�|�@��@���@��@��@��9@�Q�@�1@���@���@���@�{@�I�@�ȴ@���@��j@�Q�@�1@���@�o@���@�E�@���@�G�@��@�9X@�C�@��H@��\@��-@�7L@�V@��9@��@���@�dZ@��@��R@�5?@��^@�7L@��`@��@���@��@�t�@�
=@�~�@��@�@��@�%@���@�bN@�I�@��@��;@���@��F@���@��F@��@�z�@���@�hs@�p�@�hs@�O�@�`B@�V@��9@��9@�t�@�n�@��h@���@�V@��-@�V@���@�@�33@��@��;@�1@��@�~�@�@��T@��@�$�@��\@�33@� �@���@��`@���@��`@��@�G�@���@��R@��H@�^5@��-@��@�n�@�$�@�&�@�r�@�I�@�Z@��j@��@���@���@���@���@��@�$�@�E�@�M�@�E�@�-@��T@���@��h@��@�&�@���@���@��u@�(�@��;@��F@�\)@�33@��H@��\@�v�@�M�@�J@��@���@���@��h@�x�@�7L@��@�bN@���@��;@��w@�l�@�33@���@�ȴ@�^5@��@�@���@�@��7@�G�@��9@�b@��
@��
@���@��-1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A@�\A?�A>�+A=�A<9XA<A;�wA:�/A:ZA9�hA8�A7��A7�A6�RA4�/A3\)A1A1&�A0ZA.�!A.M�A-
=A+�mA*jA*�A(�HA'�A'K�A&�jA&�uA$�`A"-A!33A|�A��AS�Az�A��AC�A��A�A|�A�RA�#AoA��A�AoA��AVAS�A-AK�A
=AM�AƨAoA
$�A	��A�!A1'A`BA��A�Az�A�;Ax�A��A �+@��P@�V@��@��P@���@��u@�M�@��@�l�@���@�  @���@��;@�=q@�&�@�+@�1'@��@�@ݙ�@�I�@�t�@�+@ف@��m@�@Ӿw@ѡ�@υ@Ͳ-@�1@˕�@�;d@���@�r�@��
@Ǯ@��@��@ēu@§�@�Z@���@�I�@� �@�b@��;@�|�@�v�@�@�p�@��@�1@��y@�J@���@�33@�J@��@��@�1@�C�@�~�@�$�@��`@�bN@�9X@��@�|�@��@���@��@��@��9@�Q�@�1@���@���@���@�{@�I�@�ȴ@���@��j@�Q�@�1@���@�o@���@�E�@���@�G�@��@�9X@�C�@��H@��\@��-@�7L@�V@��9@��@���@�dZ@��@��R@�5?@��^@�7L@��`@��@���@��@�t�@�
=@�~�@��@�@��@�%@���@�bN@�I�@��@��;@���@��F@���@��F@��@�z�@���@�hs@�p�@�hs@�O�@�`B@�V@��9@��9@�t�@�n�@��h@���@�V@��-@�V@���@�@�33@��@��;@�1@��@�~�@�@��T@��@�$�@��\@�33@� �@���@��`@���@��`@��@�G�@���@��R@��H@�^5@��-@��@�n�@�$�@�&�@�r�@�I�@�Z@��j@��@���@���@���@���@��@�$�@�E�@�M�@�E�@�-@��T@���@��h@��@�&�@���@���@��u@�(�@��;@��F@�\)@�33@��H@��\@�v�@�M�@�J@��@���@���@��h@�x�@�7L@��@�bN@���@��;@��w@�l�@�33@���@�ȴ@�^5@��@�@���@�@��7@�G�@��9@�b@��
@��
@���@��-1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	
=B	+B		7B	B	B	  B��B��B��B�B�B�yB�mB�TB�B�B��B��B��BɺBƨB��B�qB�RB�FB�3B�!B�B�B�B��B��B�uB�+Bv�Bp�Bm�BjBhsBffBffBcTBaHB`BB`BB`BB^5B[#BZBXBT�BS�BS�BP�BN�BM�BL�BK�BH�BI�BF�BF�BD�B?}B@�B=qB9XB7LB6FB6FB5?B2-B2-B1'B-B,B+B(�B'�B%�B#�B#�B"�B"�B"�B!�B�B �B �B�B#�B�B'�B#�B�B%�B$�B�B"�B"�B"�B#�B#�B �B�B�B�B"�B �B�B%�B)�B#�B#�B#�B$�B'�B(�B'�B(�B)�B/B+B+B+B-B0!B1'B2-B5?B7LB:^B=qB?}BA�BA�BB�BC�BE�BE�BH�BK�BL�BN�BQ�BR�BVBYB[#BdZBhsBk�Bo�Bq�Br�Bs�Bu�Bw�B{�B�B�B�B�B�=B�VB�JB�\B�oB�oB�oB��B��B��B��B��B��B�B�B�-B�?B�LB�XB�XB�dB�qB��BBĜBɺB��B��B��B��B�B�)B�;B�TB�fB�B��B��B	%B	1B		7B	DB	\B	oB	{B	�B	�B	�B	�B	�B	�B	$�B	-B	2-B	49B	6FB	8RB	<jB	>wB	A�B	<jB	<jB	;dB	<jB	>wB	A�B	H�B	Q�B	]/B	_;B	^5B	_;B	bNB	cTB	ffB	x�B	~�B	}�B	y�B	z�B	�B	�B	�B	�B	�B	�+B	�PB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�3B	�?B	�FB	�LB	�^B	�qB	�qB	�wB	�}B	��B	ĜB	ĜB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�/B	�BB	�HB	�BB	�TB	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	DB	DB	DB	1B	B	B	B��B��B��B�B�B�yB�sB�5B�B��B��B��B��BɺBĜB��B�XB�XB�?B�-B�!B�B�!B�B��B��B�\B{�Bs�Bo�Bl�BjBhsBgmBffBdZBcTBdZBcTBcTB\)B[#BZBXBVBT�BR�BP�BO�BN�BM�BK�BJ�BH�BH�BF�BC�BB�B?}B>wB:^B8RB8RB7LB5?B49B33B0!B-B-B,B+B(�B'�B%�B#�B%�B'�B&�B#�B#�B"�B!�B#�B�B(�B&�B�B'�B&�B"�B$�B#�B#�B$�B&�B!�B�B�B �B$�B#�B�B(�B,B$�B$�B$�B%�B)�B)�B(�B)�B,B0!B,B-B.B/B1'B2-B33B6FB8RB:^B?}B@�BA�BB�BC�BD�BF�BG�BJ�BL�BM�BO�BR�BS�BW
BZB_;BgmBjBl�Bo�Br�Bs�Bt�Bv�Bx�B|�B�B�B�B�%B�DB�VB�PB�bB�oB�oB�uB��B��B��B��B��B��B�B�!B�3B�FB�RB�XB�^B�jB�wB��BÖBŢB��B��B��B��B��B�B�)B�;B�TB�fB�B��B��B	+B	1B		7B	DB	bB	uB	{B	�B	�B	�B	�B	�B	�B	#�B	,B	2-B	49B	5?B	8RB	<jB	?}B	B�B	<jB	<jB	;dB	<jB	=qB	@�B	G�B	P�B	]/B	_;B	^5B	_;B	bNB	cTB	e`B	x�B	� B	~�B	y�B	z�B	�B	�B	�B	�B	�B	�%B	�PB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�3B	�?B	�FB	�RB	�dB	�qB	�wB	�}B	��B	B	ĜB	ĜB	ŢB	ǮB	ǮB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�5B	�BB	�HB	�BB	�TB	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<D��<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<D��<T��<49X<D��<T��<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<D��<49X<#�
<49X<49X<49X<#�
<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<#�
<#�
<49X<49X<49X<49X<49X<49X<49X<49X<#�
<#�
<49X<49X<#�
<#�
<#�
<#�
<49X<49X<49X<49X<49X<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<49X<49X<49X<49X<49X<49X<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<49X<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<49X<49X<49X<#�
<49X<49X<49X<#�
<#�
<#�
<#�
<#�
<49X<49X<49X<#�
<#�
<#�
<#�
<49X<49X<49X<#�
<49X<#�
<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49X<49XPRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201112181657252011121816572520111218165725  AO  ARGQ                                                                        20111130103316  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130103316  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20111218165725  IP                  G�O�G�O�G�O�                