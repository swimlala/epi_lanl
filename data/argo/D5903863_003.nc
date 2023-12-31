CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2014-07-21T23:38:43Z creation; 2014-07-21T23:38:43Z updated; 2015-09-28T12:13:23Z converted from 3.0   
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7    PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7`   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8    PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8$   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8D   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8d   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           8h   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8p   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8t   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8|   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        =���   axis      Z        H  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  G�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  K�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  ]`   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  k�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  }�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  �`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  Ϩ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ـ   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    لArgo profile    3.1 1.2 19500101000000  20140721233843  20170523133322  5903863 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  4298_0127_003                   2C  D   NAVIS_A                         0127                            120111                          863 @�'a' 1   @�'a⛀ @5���`A��d�ȴ9X1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @s33@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B�fgB���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٙC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3Dws3Dw�3Dxs3Dx�3Dys3Dy�3Dzs3Dz�3D{s3D{�3D|s3D|�3D}s3D}�3D~s3D~�3Ds3D�3D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D๚D���D�9�D�y�DṚD���D�9�D�y�D⹚D���D�9�D�y�D㹚D���D�9�D�|�D� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�7LA�?}A�A�A�E�A�?}A�5?A�1'A�/A�7LA�=qA�;dA�G�A�I�A�M�A�S�A�S�A�ZA�ZA�\)A�`BA�bNA�dZA�dZA�dZA�`BA�dZA�ffA�dZA�ffA�ffA�ffA�hsA�hsA�ffA�hsA�hsA�jA�jA�l�A�l�A�n�A�l�A�5?A�JA��A�/A�XA�
=A�A��A�z�A��!A�Q�A�dZA��A���A��A��9A�dZA��-A�G�A�M�A���A�O�A��A�A��DA��FA��A�7LA�ffA��DA���A��A�A�G�A���A��
A�M�A��A��A�K�A��9A��mA���A�&�A�dZA���A���A�$�A��A��
A��hA��A��A��jA�C�A�|�A�bA��\A�v�A�VA��#A��wA�1'A��yA���A��DA�A�n�A�VA�ĜA�z�A�oA�ȴA�(�A~ZA}/A|�A|z�A{p�AyƨAx��Aw�wAvZAu��AsAoK�Al��Ak�Ak�
Akl�Aj{Ai�AhZAg�7Af��Ae��Ae�^Ad�+Ac/Ab^5Aa�A`JA^��A]��A\��A[�AXM�AV5?AU��AU�PATbNASl�AR�ARbAQ\)AP�9APA�AO��AO�AL�9AKhsAJ��AJ�AI��AI+AHbAE�mAC��AB��AA��AA"�A?�A?33A>M�A>�A=��A<�DA;�wA;K�A9�;A89XA7XA6�jA6�A5�-A5\)A4�A4=qA3�7A2�A0�A-�A*�+A)
=A'��A%x�A$n�A"��A!t�A �AXA�HA�uA=qA�A;dAA�A��A�#AA�A33AJA\)A�A%A�/Az�A��AĜA~�AbNAI�A$�A��A;dA
�A
E�A	�mA	�
Ar�Ap�A/A�AffAz�A�AC�A�+A�TAG�A ȴ@��@��@�=q@��T@��-@���@�hs@�?}@�V@�  @���@��@��
@�R@��#@�ƨ@�@�?}@�j@畁@�V@�@�O�@�dZ@��`@۶F@��@��y@���@�{@ؓu@׮@�J@�?}@Լj@�\)@�?}@���@��@͉7@�V@�r�@� �@��m@˝�@�t�@�v�@ɲ-@�/@�(�@�\)@�n�@ũ�@��@�r�@���@��H@�V@���@�X@��`@�1@�C�@��+@�hs@��@�Z@���@�z�@�Z@�9X@��F@�33@�V@���@��@��;@�33@�ȴ@�`B@���@��@�v�@�E�@�J@��@���@�x�@��h@��7@��@���@���@�1@���@�K�@�@��H@���@���@�v�@�E�@�$�@�@��T@��h@��@��D@��w@��@�ff@�J@���@�p�@��@���@��@�Ĝ@� �@�ƨ@�S�@�o@���@�M�@��-@�X@���@�(�@���@�l�@�K�@�;d@�33@�+@��@���@��H@��!@�~�@�n�@�V@�-@���@��@���@�Z@�1@��w@��@��y@�V@�-@��T@�G�@��@�%@��@��/@��/@���@�r�@�b@��F@�l�@�o@�ȴ@���@�hs@��@�?}@�G�@�O�@�/@��@��/@���@��/@��@�1'@��@�Q�@�A�@�1'@�1'@��@��P@���@�C�@��@�-@��@�@�X@� �@��F@���@���@�|�@�l�@�K�@�33@��@���@��R@��!@���@���@���@�~�@�v�@�v�@�v�@�ff@�V@��@��T@�?}@��/@��j@��j@���@���@���@��@���@���@���@��`@��j@��@�r�@�bN@�j@�z�@��u@�9X@�b@���@���@�+@�33@���@���@���@�M�@���@���@��h@��@�G�@��@���@���@��j@���@��@�1'@��@��w@�|�@�S�@�@��!@��+@�v�@�{@��@���@���@���@���@���@���@��h@��h@��@�x�@�hs@�/@���@��j@�Q�@�1'@�b@�  @��m@��@�"�@�~�@���@�`B@��@�V@�%@���@��D@��D@��@�9X@��@;d@~�+@~@}p�@|�@|9X@{��@{C�@{C�@{C�@{33@z�\@z�@y��@y�^@yX@x�`@w��@v��@vV@u�@uO�@u/@u/@u/@u�@t�/@tz�@t(�@s�F@so@r�!@rn�@r=q@q�#@q&�@p�@pbN@o��@n��@n�R@nE�@n@m��@m`B@m?}@l�j@l(�@k�F@j�@i��@i�^@ix�@i�7@i�7@ix�@ihs@ihs@h��@hĜ@hĜ@h��@hĜ@h�u@h�@hbN@h1'@g�;@gK�@g�@f�y@f��@fv�@fE�@f@e��@e`B@d�/@d9X@c�@cC�@bn�@a��@a�7@a%@` �@_\)@^v�@]��@]�@\�/@\Z@\9X@\I�@\I�@\I�@\9X@\9X@\�@[�F@["�@Z��@Y��@Y��@Y�7@Yx�@YX@X�u@XbN@Xb@W��@W\)@W
=@V��@Vv�@Vff@V@U�h@U?}@T�@T��@Tj@TZ@T9X@T(�@T(�@T(�@T�@T�@T1@S��@S��@S�@SdZ@SS�@SS�@SS�@R�@RM�@Q�#@Q7L@Q�@Q%@P��@PA�@O��@O�@O�P@O\)@O�@Nff@N$�@M��@M�h@M`B@MO�@M�@LI�@L1@K�m@K�F@KdZ@K@J�!@I�^@I7L@H�u@G�;@Gl�@F�y@F��@Fv�@E�T@E�h@E`B@E�@E�@Dz�@D(�@C�
@Ct�@C"�@B�@B��@B~�@B^5@BM�@A��@@�`@@��@@�@?�w@?|�@?K�@?;d@?+@>v�@>@=p�@=�@<I�@;�m@;��@;"�@:�\@:=q@9��@9�7@97L@8�`@8��@8b@7�@6��@6�+@6{@5p�@5?}@5/@4�j@4(�@3�
@3��@3t�@3o@2=q@1X@0��@0�9@0A�@/�;@/��@/+@.ȴ@.��@.v�@-�@-�@,��@,z�@+�@+33@*��@*M�@*-@*J@)�#@)��@)��@)��@)��@)x�@)hs@)hs@)G�@)&�@(Ĝ@(1'@'�@'��@'�w@'��@'\)@';d@'+@';d@&��@&��@&��@&ȴ@&�+@&{@%��@%�@%?}@%/@%V@$�D@$Z@$I�@#�m@#@"~�@"=q@"=q@"-@"J@!��@!G�@!�@ ��@ ��@ �9@ A�@��@|�@\)@K�@;d@;d@K�@;d@�@��@��@�+@�+@v�@�T@�-@O�@��@I�@ƨ@��@dZ@@n�@-@J@��@��@X@G�@7L@7L@&�@�@��@��@�u@r�@A�@b@�;@�@|�@l�@l�@�P@��@�P@l�@K�@+@��@ȴ@�+@E�@$�@$�@$�@$�@{@�@��@@��@?}@V@�@z�@9X@��@�m@ƨ@�F@��@t�@33@o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�7LA�?}A�A�A�E�A�?}A�5?A�1'A�/A�7LA�=qA�;dA�G�A�I�A�M�A�S�A�S�A�ZA�ZA�\)A�`BA�bNA�dZA�dZA�dZA�`BA�dZA�ffA�dZA�ffA�ffA�ffA�hsA�hsA�ffA�hsA�hsA�jA�jA�l�A�l�A�n�A�l�A�5?A�JA��A�/A�XA�
=A�A��A�z�A��!A�Q�A�dZA��A���A��A��9A�dZA��-A�G�A�M�A���A�O�A��A�A��DA��FA��A�7LA�ffA��DA���A��A�A�G�A���A��
A�M�A��A��A�K�A��9A��mA���A�&�A�dZA���A���A�$�A��A��
A��hA��A��A��jA�C�A�|�A�bA��\A�v�A�VA��#A��wA�1'A��yA���A��DA�A�n�A�VA�ĜA�z�A�oA�ȴA�(�A~ZA}/A|�A|z�A{p�AyƨAx��Aw�wAvZAu��AsAoK�Al��Ak�Ak�
Akl�Aj{Ai�AhZAg�7Af��Ae��Ae�^Ad�+Ac/Ab^5Aa�A`JA^��A]��A\��A[�AXM�AV5?AU��AU�PATbNASl�AR�ARbAQ\)AP�9APA�AO��AO�AL�9AKhsAJ��AJ�AI��AI+AHbAE�mAC��AB��AA��AA"�A?�A?33A>M�A>�A=��A<�DA;�wA;K�A9�;A89XA7XA6�jA6�A5�-A5\)A4�A4=qA3�7A2�A0�A-�A*�+A)
=A'��A%x�A$n�A"��A!t�A �AXA�HA�uA=qA�A;dAA�A��A�#AA�A33AJA\)A�A%A�/Az�A��AĜA~�AbNAI�A$�A��A;dA
�A
E�A	�mA	�
Ar�Ap�A/A�AffAz�A�AC�A�+A�TAG�A ȴ@��@��@�=q@��T@��-@���@�hs@�?}@�V@�  @���@��@��
@�R@��#@�ƨ@�@�?}@�j@畁@�V@�@�O�@�dZ@��`@۶F@��@��y@���@�{@ؓu@׮@�J@�?}@Լj@�\)@�?}@���@��@͉7@�V@�r�@� �@��m@˝�@�t�@�v�@ɲ-@�/@�(�@�\)@�n�@ũ�@��@�r�@���@��H@�V@���@�X@��`@�1@�C�@��+@�hs@��@�Z@���@�z�@�Z@�9X@��F@�33@�V@���@��@��;@�33@�ȴ@�`B@���@��@�v�@�E�@�J@��@���@�x�@��h@��7@��@���@���@�1@���@�K�@�@��H@���@���@�v�@�E�@�$�@�@��T@��h@��@��D@��w@��@�ff@�J@���@�p�@��@���@��@�Ĝ@� �@�ƨ@�S�@�o@���@�M�@��-@�X@���@�(�@���@�l�@�K�@�;d@�33@�+@��@���@��H@��!@�~�@�n�@�V@�-@���@��@���@�Z@�1@��w@��@��y@�V@�-@��T@�G�@��@�%@��@��/@��/@���@�r�@�b@��F@�l�@�o@�ȴ@���@�hs@��@�?}@�G�@�O�@�/@��@��/@���@��/@��@�1'@��@�Q�@�A�@�1'@�1'@��@��P@���@�C�@��@�-@��@�@�X@� �@��F@���@���@�|�@�l�@�K�@�33@��@���@��R@��!@���@���@���@�~�@�v�@�v�@�v�@�ff@�V@��@��T@�?}@��/@��j@��j@���@���@���@��@���@���@���@��`@��j@��@�r�@�bN@�j@�z�@��u@�9X@�b@���@���@�+@�33@���@���@���@�M�@���@���@��h@��@�G�@��@���@���@��j@���@��@�1'@��@��w@�|�@�S�@�@��!@��+@�v�@�{@��@���@���@���@���@���@���@��h@��h@��@�x�@�hs@�/@���@��j@�Q�@�1'@�b@�  @��m@��@�"�@�~�@���@�`B@��@�V@�%@���@��D@��D@��@�9X@��@;d@~�+@~@}p�@|�@|9X@{��@{C�@{C�@{C�@{33@z�\@z�@y��@y�^@yX@x�`@w��@v��@vV@u�@uO�@u/@u/@u/@u�@t�/@tz�@t(�@s�F@so@r�!@rn�@r=q@q�#@q&�@p�@pbN@o��@n��@n�R@nE�@n@m��@m`B@m?}@l�j@l(�@k�F@j�@i��@i�^@ix�@i�7@i�7@ix�@ihs@ihs@h��@hĜ@hĜ@h��@hĜ@h�u@h�@hbN@h1'@g�;@gK�@g�@f�y@f��@fv�@fE�@f@e��@e`B@d�/@d9X@c�@cC�@bn�@a��@a�7@a%@` �@_\)@^v�@]��@]�@\�/@\Z@\9X@\I�@\I�@\I�@\9X@\9X@\�@[�F@["�@Z��@Y��@Y��@Y�7@Yx�@YX@X�u@XbN@Xb@W��@W\)@W
=@V��@Vv�@Vff@V@U�h@U?}@T�@T��@Tj@TZ@T9X@T(�@T(�@T(�@T�@T�@T1@S��@S��@S�@SdZ@SS�@SS�@SS�@R�@RM�@Q�#@Q7L@Q�@Q%@P��@PA�@O��@O�@O�P@O\)@O�@Nff@N$�@M��@M�h@M`B@MO�@M�@LI�@L1@K�m@K�F@KdZ@K@J�!@I�^@I7L@H�u@G�;@Gl�@F�y@F��@Fv�@E�T@E�h@E`B@E�@E�@Dz�@D(�@C�
@Ct�@C"�@B�@B��@B~�@B^5@BM�@A��@@�`@@��@@�@?�w@?|�@?K�@?;d@?+@>v�@>@=p�@=�@<I�@;�m@;��@;"�@:�\@:=q@9��@9�7@97L@8�`@8��@8b@7�@6��@6�+@6{@5p�@5?}@5/@4�j@4(�@3�
@3��@3t�@3o@2=q@1X@0��@0�9@0A�@/�;@/��@/+@.ȴ@.��@.v�@-�@-�@,��@,z�@+�@+33@*��@*M�@*-@*J@)�#@)��@)��@)��@)��@)x�@)hs@)hs@)G�@)&�@(Ĝ@(1'@'�@'��@'�w@'��@'\)@';d@'+@';d@&��@&��@&��@&ȴ@&�+@&{@%��@%�@%?}@%/@%V@$�D@$Z@$I�@#�m@#@"~�@"=q@"=q@"-@"J@!��@!G�@!�@ ��@ ��@ �9@ A�@��@|�@\)@K�@;d@;d@K�@;d@�@��@��@�+@�+@v�@�T@�-@O�@��@I�@ƨ@��@dZ@@n�@-@J@��@��@X@G�@7L@7L@&�@�@��@��@�u@r�@A�@b@�;@�@|�@l�@l�@�P@��@�P@l�@K�@+@��@ȴ@�+@E�@$�@$�@$�@$�@{@�@��@@��@?}@V@�@z�@9X@��@�m@ƨ@�F@��@t�@33@o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BL�BL�BL�BJ�BI�BD�B?}B:^B33B.B)�B%�B�B�BoBPB%BB��B��B�sB�BB�
B��B��B�RB�'B��B�uB�JB~�Bp�BdZB[#BT�BP�BL�BG�B=qB5?B.B%�B�BVB1B�B�HB�B�}B�!B��B�PB�Bq�BaHBK�B?}B+B�B1B
��B
�`B
�B
��B
��B
ȴB
ĜB
�jB
�9B
�B
�B
��B
��B
��B
�\B
w�B
n�B
jB
hsB
aHB
W
B
N�B
H�B
?}B
8RB
%�B
VB	��B	��B	��B	�B	�B	�yB	�NB	�#B	�B	��B	��B	��B	B	�jB	�LB	�!B	��B	��B	��B	�DB	x�B	k�B	iyB	ffB	aHB	\)B	W
B	T�B	R�B	O�B	M�B	K�B	F�B	:^B	2-B	.B	,B	)�B	&�B	 �B	�B	JB	+B	B	  B��B��B��B��B��B��B�B�B�NB�#B�
B��B��B��B��B��B��BɺBŢB�}B�LB�'B�B��B��B��B��B��B��B��B�{B�uB�hB�VB�DB�7B�+B�B�B� B}�B{�By�Bx�Bw�Bu�Bs�Br�Bp�Bo�Bo�Bn�Bl�Bk�BiyBhsBgmBffBdZBcTBdZBe`BhsBbNBcTBbNBcTBbNBe`BffBffBgmBgmBffBe`Be`BdZBcTBaHB[#BT�BO�BN�BN�BN�BO�BO�BN�BM�BM�BL�BP�BO�BP�BR�BT�BVBVBVBVBW
BW
BYBZBZB\)B_;BcTBe`BffBhsBiyBjBjBk�BjBl�Bm�Br�Bs�Bv�Bw�Bw�Bv�Bw�By�Bz�Bz�B~�B�B�%B�%B�+B�7B�JB�PB�VB�{B��B��B��B��B��B��B��B��B��B��B��B��B�!B�?B�RB�XB�^B�dB�wB��B��B��B��B��B��B��B��B�B�
B�B�B�B�B�B�B�#B�)B�5B�HB�ZB�B�B�B�B�B��B��B��B��B��B	B	B	+B	
=B	\B	�B	�B	�B	#�B	&�B	(�B	+B	,B	-B	.B	.B	/B	0!B	1'B	2-B	33B	49B	49B	6FB	9XB	@�B	E�B	G�B	I�B	N�B	Q�B	T�B	W
B	XB	ZB	_;B	aHB	bNB	cTB	cTB	cTB	dZB	ffB	iyB	k�B	l�B	n�B	n�B	o�B	p�B	r�B	v�B	x�B	x�B	x�B	w�B	w�B	w�B	w�B	x�B	x�B	|�B	�B	�B	�B	�B	�+B	�+B	�%B	�=B	�=B	�7B	�DB	�DB	�7B	�1B	�7B	�=B	�=B	�=B	�DB	�JB	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�9B	�?B	�LB	�XB	�^B	�jB	�wB	�}B	��B	��B	B	ÖB	ƨB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	�B	�/B	�5B	�;B	�;B	�BB	�NB	�NB	�TB	�TB	�ZB	�`B	�`B	�fB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
1B
	7B
	7B

=B
DB
DB
DB
DB
DB
DB
DB
DB
DB
DB
PB
PB
VB
bB
hB
hB
hB
hB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
&�B
%�B
&�B
&�B
&�B
%�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
)�B
)�B
+B
)�B
)�B
)�B
)�B
,B
,B
-B
-B
.B
/B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
L�B
K�B
K�B
J�B
I�B
I�B
J�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
M�B
N�B
M�B
L�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
VB
T�B
VB
T�B
VB
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gm11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BM�BM�BM�BNBNBM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BMeBNABQ�BFCBFBG�B<�B1�B+�B*B$�B�B�BpB�B$BB�qB�B�RB�B�B��B�?B�[B�OB�iB��B��BtUBgB^;BWTBUEBO}BK�B@oB8�B15B*~B�B=BGB��B�B�PB�WB�B�"B�"B�Bt�Be�BNFBC�B/�B/B�B
��B
�B
�B
��B
�CB
��B
��B
��B
��B
�pB
�NB
��B
�.B
��B
�B
z�B
o�B
k%B
k3B
e|B
ZB
Q;B
LAB
A�B
?ZB
/;B
1B	��B	�(B	�)B	�{B	�<B	�B	�B	�B	�dB	��B	�B	�6B	��B	��B	�B	��B	��B	�OB	�B	�3B	}�B	l�B	jQB	iZB	c�B	^�B	X>B	V�B	T�B	Q!B	O*B	M�B	L�B	>B	4dB	/�B	-�B	+mB	*,B	'B	�B	�B	
EB	(B	�B�,B��B�wB�mB�/B�B�HB� B�OB��B��B��B�>B��B�0B��BͬB�CBʅBŞB�~B�RB��B��B��B��B��B�eB�?B��B�7B�SB�(B��B��B��B�B�MB��B�/B�BdB|�BydBy>B{�By-Bs�BqBp
Bp)Bo_Bn�Bm�BjPBi�Bg�Bj�BgbBd,Be>Bg;Bm�Bc�Be�Bd�BeLBdBf�BjBl6BhcBhBf�Be�Be�Bd�Bc�BbfB]�BY�BQ�BP>BP BQBRBP�BO�BN�BO�BR�BQ�BRyBT+BT�BU�BV]BVFBW#BX BXsBYgBZABZ�B\B_%BbBd�BfBg)BihBi�Bj�Bj�Bk�Bk�Bm�BneBtBt�Bx#Bx�Bx�Bw�Bx�B{XB}4B{
B~�B��B�KB�@B�?B��B� B�3B��B�|B��B��B�YB�bB��B�xB��B��B��B�_B�~B��B�=B��B��B��B��B��B��B��B��BҘB�B�OB��B՛B՗BփB�OB�XB�MB�iB�qB�]B�fB�fBܱB��B�+B�B��B�qB�8B�DB�B�KB�!B�B�2B��B	�B	�B	�B	
�B	�B	XB	8B	�B	$�B	'�B	)PB	+DB	,4B	-5B	.9B	.@B	/`B	0\B	1�B	2�B	3`B	4uB	4�B	6�B	:[B	AHB	FB	H0B	J1B	ODB	R�B	U�B	W`B	X�B	[B	_�B	a�B	b�B	c�B	csB	c�B	d�B	gB	j$B	lB	m)B	oB	o�B	p`B	qB	r�B	v�B	x�B	yB	y<B	w�B	w�B	w�B	x+B	y�B	x�B	}GB	�=B	�JB	�=B	��B	��B	�B	��B	��B	�7B	�gB	��B	�MB	� B	��B	�kB	�nB	�zB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�3B	�B	��B	��B	�YB	�MB	�hB	�uB	�]B	��B	��B	��B	��B	��B	�B	��B	��B	ƷB	ǱB	��B	�vB	�3B	�&B	�TB	��B	�+B	ݢB	ފB	ߕB	��B	�GB	�B	�tB	�B	��B	�B	�B	�B	��B	�B	��B	�	B	��B	��B	� B	��B	�B	�$B	��B	��B	�IB	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�3B	�.B	�8B	�vB	�B	�"B	�B	�$B	�OB	��B	��B	��B
 �B
~B
0B
4B
oB
xB
+B
=B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B

�B
]B
`B
jB
�B
�B
tB
�B
�B
�B
B
 B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
*B
B
�B
!B
B
�B
�B
�B
�B
�B
�B
B
+B
B
MB
jB
�B
 B
�B
�B
�B
�B
 �B
 B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
"B
""B
"IB
"B
#B
#B
#B
#B
#!B
#B
#@B
$OB
$eB
%wB
%*B
%�B
&�B
'B
&dB
'�B
'�B
'�B
&�B
%WB
%*B
%PB
%B
$�B
$�B
%�B
&	B
%�B
'B
'KB
(B
*�B
*�B
+BB
*%B
*&B
*5B
*�B
,MB
,hB
-bB
-�B
.sB
/�B
/LB
/EB
/�B
/�B
0}B
0�B
0{B
1dB
1QB
1\B
1OB
1DB
1BB
1OB
1GB
1VB
1�B
2UB
2XB
2_B
2SB
2DB
2IB
2�B
2�B
2�B
3�B
3fB
2VB
2]B
2�B
2�B
2_B
3fB
3vB
3�B
3�B
4�B
5�B
5�B
5�B
5jB
5�B
6�B
7�B
7�B
7�B
8�B
8�B
8�B
9B
9�B
:�B
:�B
;�B
;�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
>B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
A;B
A�B
A�B
C1B
B�B
C�B
C�B
C�B
D2B
DB
E!B
D�B
EOB
EB
E�B
FB
F B
E�B
E�B
F�B
G�B
G�B
G�B
H B
HZB
H�B
IB
JB
J<B
J�B
J�B
K)B
L@B
LB
MB
K�B
L&B
KqB
JtB
JPB
J�B
M:B
M0B
NB
N?B
N2B
NB
OB
O[B
O�B
O2B
N1B
O�B
N(B
M1B
N?B
O	B
OB
OB
N�B
O
B
N�B
PB
PB
PB
O�B
PB
PB
P=B
P]B
Q+B
QB
Q	B
QB
Q(B
QB
QB
P�B
Q)B
Q B
P�B
QB
Q,B
QNB
S8B
TIB
T<B
TB
T-B
TpB
T5B
T B
T[B
T�B
UtB
UDB
VB
U%B
V6B
U\B
VaB
V?B
W<B
W=B
W>B
WxB
WrB
XfB
XCB
X5B
Y>B
Y3B
Y!B
Y=B
YtB
YUB
ZDB
ZDB
Z9B
ZGB
Z�B
ZYB
Z�B
[�B
[�B
[�B
\]B
\rB
\�B
]�B
]tB
^eB
^[B
^�B
^�B
_eB
__B
_XB
_cB
_cB
_lB
`uB
`�B
`tB
`}B
`�B
a�B
a�B
a�B
amB
acB
bVB
baB
buB
b�B
bB
bB
b�B
c�B
c�B
b�B
b~B
cmB
csB
clB
cyB
c�B
c�B
c~B
c�B
c�B
d�B
d�B
e�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<b�|<,�><#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<&`�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<&�<#�
<#�
<)��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
</B�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<*y(<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.2 dbar                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201603101134352016031011343520160310113435  AO  ARCAADJP                                                                    20140721233843    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721233843  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721233843  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20160310113435  QC  PRES            @�  D�fG�O�                PM  ARSQCTM V1.1                                                                20160310113435  QC  PSAL            @�  D�fG�O�                PM  ARSQOWGUV1.0WOD + Argo                                                      20170523133322  IP                  G�O�G�O�G�O�                