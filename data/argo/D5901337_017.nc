CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:28Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112602  20190522121836  1901_5055_017                   2C  D   APEX                            2140                            040306                          846 @�S�A�1   @�S����	@/��$��cw���+1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBi33Bo��Bw��B��B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:y�D;  D;�fD<fD<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD�fDE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DI��DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZfDZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dvy�DyS3D�3D�L�D�s3D��3D���D�3D�Y�D�� D���D�,�D�c3D��3D���D�0 D�ffD��3D�� D�  D�P D�c3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_��BhffBn��Bv��B~��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C�fC��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=�fC?��CA��CC��CE��CG��CI��CK��CM��CO�3CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq�fCs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:l�D:�3D;y�D;��D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC��DDy�DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI��DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY��DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvl�DyFfD���D�FfD�l�D���D��fD��D�S3D���D��3D�&fD�\�DǼ�D��3D�)�D�` D��D�ٚD��D�I�D�\�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��yA��A��A�  A�  A�  A���A�  A�  A�A�A�A�1A�1A�1A�
=A�bA�$�A��A�  A��A�ƨAʺ^Aʧ�A�v�A�XA�AŴ9A�\)A��\A�M�A���A�r�A��A�=qA�bA���A�S�A���A��A�=qA�=qA�A�oA�bA���A�E�A�^5A�Q�A��yA�=qA���A�|�A��jA��A�G�A�dZA��-A�bNA�r�A��DA���A�I�A�C�A��-A�5?A�bNA�A�hsA��
A��^A���A�A��;A��+A�ffA�z�A�^5A}7LAzjAu��Ar��An��Ai��Af�yAe�PAdI�Aa?}A\�HAZ-AY33AXbNAW�TAU�AT1AR�ANA�AJ��AF��AC�AA��A>�DA>�yA>�A>��A>�+A=�^A;��A:�A:Q�A9��A5��A4A2 �A0��A/ƨA/l�A.�/A.�uA.bA.1A,�HA+�A+?}A*�A*E�A)XA'��A'�A%�A#��A"�RA"I�A �!A ffAS�A��A�\A �A9XA(�A�A�A��A��AZAbA�wAK�A��An�AS�A��Av�AJA��A;dA�^A�
A��A�PA&�A��AVA�FA"�A
=A�A��AVA9XA$�A1'A-A�TA��A�A��A|�A�A\)A�wA�A��A�A�A$�An�A��A�AG�A
=AoA	��A	XA�A�AI�A��AVA�7A��A jA {@�l�@�~�@�v�@�V@��@�V@���@�%@���@��@�b@��\@�G�@�p�@��-@��@��9@�I�@���@���@���@�Ĝ@�"�@��T@�Ĝ@�F@�R@�$�@��T@�7@�Ĝ@�bN@��@��y@�+@�ff@�5?@�/@蛦@�@�
=@�"�@�@���@�{@�p�@�Z@�t�@���@�5?@噚@�G�@�h@���@�b@�P@�S�@��@�+@�~�@�^5@�{@�p�@�@�A�@߶F@�;d@އ+@��@�%@۝�@�o@ڟ�@�v�@�E�@�p�@�/@؛�@���@׍P@�S�@�"�@��@և+@�-@���@ӶF@Ӆ@�l�@�+@���@�`B@�?}@�X@��`@���@��@��/@� �@ϕ�@�dZ@�o@Ώ\@�ff@�$�@�@��T@͙�@�O�@�1'@�C�@�o@���@���@ʗ�@ʏ\@�ff@�{@ə�@�%@�r�@ǍP@�o@�@�ȴ@��@��;@��@�5?@���@�7L@��;@�o@��@�ȴ@��!@�v�@�=q@�hs@�I�@���@���@���@���@��@�S�@�o@��H@�E�@���@���@��j@��@�I�@�1'@��@��@���@���@�-@�@���@���@���@��h@�/@�  @��P@�n�@�{@��#@�`B@��/@�Ĝ@�r�@�(�@�9X@�9X@��@�l�@���@�\)@�33@��R@��@���@�hs@�`B@�O�@�/@�V@�%@��@���@�v�@�-@�5?@�M�@��R@�=q@��@��@�1@��@��@�t�@�+@��@��/@��F@���@�|�@�S�@��@��@�o@���@��y@��y@���@�C�@�C�@��@�n�@���@��!@���@���@�hs@�&�@�9X@�t�@�;d@��!@��T@�x�@��@���@���@�K�@�@���@��H@���@�$�@���@��@���@��@��@�9X@��@��
@��F@���@��@�S�@�
=@�M�@���@��7@��@��j@���@�A�@�(�@�1@���@���@�t�@�dZ@�33@�@��H@��!@�ff@�V@�M�@�M�@��@��7@�O�@��@��@���@�z�@�@��9@��@z��@nE�@d��@]�T@V@P  @G�P@@��@6��@0A�@+dZ@'|�@!%@��@��@o@ff@
^5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��yA��A��A�  A�  A�  A���A�  A�  A�A�A�A�1A�1A�1A�
=A�bA�$�A��A�  A��A�ƨAʺ^Aʧ�A�v�A�XA�AŴ9A�\)A��\A�M�A���A�r�A��A�=qA�bA���A�S�A���A��A�=qA�=qA�A�oA�bA���A�E�A�^5A�Q�A��yA�=qA���A�|�A��jA��A�G�A�dZA��-A�bNA�r�A��DA���A�I�A�C�A��-A�5?A�bNA�A�hsA��
A��^A���A�A��;A��+A�ffA�z�A�^5A}7LAzjAu��Ar��An��Ai��Af�yAe�PAdI�Aa?}A\�HAZ-AY33AXbNAW�TAU�AT1AR�ANA�AJ��AF��AC�AA��A>�DA>�yA>�A>��A>�+A=�^A;��A:�A:Q�A9��A5��A4A2 �A0��A/ƨA/l�A.�/A.�uA.bA.1A,�HA+�A+?}A*�A*E�A)XA'��A'�A%�A#��A"�RA"I�A �!A ffAS�A��A�\A �A9XA(�A�A�A��A��AZAbA�wAK�A��An�AS�A��Av�AJA��A;dA�^A�
A��A�PA&�A��AVA�FA"�A
=A�A��AVA9XA$�A1'A-A�TA��A�A��A|�A�A\)A�wA�A��A�A�A$�An�A��A�AG�A
=AoA	��A	XA�A�AI�A��AVA�7A��A jA {@�l�@�~�@�v�@�V@��@�V@���@�%@���@��@�b@��\@�G�@�p�@��-@��@��9@�I�@���@���@���@�Ĝ@�"�@��T@�Ĝ@�F@�R@�$�@��T@�7@�Ĝ@�bN@��@��y@�+@�ff@�5?@�/@蛦@�@�
=@�"�@�@���@�{@�p�@�Z@�t�@���@�5?@噚@�G�@�h@���@�b@�P@�S�@��@�+@�~�@�^5@�{@�p�@�@�A�@߶F@�;d@އ+@��@�%@۝�@�o@ڟ�@�v�@�E�@�p�@�/@؛�@���@׍P@�S�@�"�@��@և+@�-@���@ӶF@Ӆ@�l�@�+@���@�`B@�?}@�X@��`@���@��@��/@� �@ϕ�@�dZ@�o@Ώ\@�ff@�$�@�@��T@͙�@�O�@�1'@�C�@�o@���@���@ʗ�@ʏ\@�ff@�{@ə�@�%@�r�@ǍP@�o@�@�ȴ@��@��;@��@�5?@���@�7L@��;@�o@��@�ȴ@��!@�v�@�=q@�hs@�I�@���@���@���@���@��@�S�@�o@��H@�E�@���@���@��j@��@�I�@�1'@��@��@���@���@�-@�@���@���@���@��h@�/@�  @��P@�n�@�{@��#@�`B@��/@�Ĝ@�r�@�(�@�9X@�9X@��@�l�@���@�\)@�33@��R@��@���@�hs@�`B@�O�@�/@�V@�%@��@���@�v�@�-@�5?@�M�@��R@�=q@��@��@�1@��@��@�t�@�+@��@��/@��F@���@�|�@�S�@��@��@�o@���@��y@��y@���@�C�@�C�@��@�n�@���@��!@���@���@�hs@�&�@�9X@�t�@�;d@��!@��T@�x�@��@���@���@�K�@�@���@��H@���@�$�@���@��@���@��@��@�9X@��@��
@��F@���@��@�S�@�
=@�M�@���@��7@��@��j@���@�A�@�(�@�1@���@���@�t�@�dZ@�33@�@��H@��!@�ff@�V@�M�@�M�@��@��7@�O�@��@��@���@�z�@�@��9@��@z��@nE�@d��@]�T@V@P  @G�P@@��@6��@0A�@+dZ@'|�@!%@��@��@o@ff@
^5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	ŢB	ŢB	ƨB	ŢB	ŢB	ĜB	ĜB	ĜB	ĜB	ŢB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ǮB	��B
{B
1'B
;dB
9XB
;dB
<jB
=qB
=qB
A�B
R�B
hsB
��B
��B
ÖB
ǮB
ÖB
�jB
�RB
B
�HB
�NB
�fB
�B
�B
�B
�mB
�B
�mBBO�B^5Bt�Bp�B`BBXBB�B8RB7LB33B#�B1B
��B
�B+BS�BB�BA�B\)B_;BI�B)�BVB
��B
��B
�'B
e`B
bB	�
B	��B	��B	�+B	s�B	XB	B�B	'�B	�B	!�B	�B	�B	+B�B�5B�#B�
B��B��B��B��B�^B��B��B��B�BɺB�B�NB	�B	;dB	H�B	H�B	H�B	M�B	O�B	J�B	B�B	:^B	?}B	G�B	L�B	N�B	XB	aHB	o�B	� B	�DB	��B	�B	�}B	��B	�B	�#B	��B	B	ÖB	ĜB	�wB	�qB	�wB	ŢB	ɺB	��B	�NB	�fB	�B	�B	�B	�B	�B	��B	��B	��B
B
B
  B	��B	��B	��B	��B	��B	��B
B
B
+B
%B
	7B
DB
JB
PB
\B
\B
\B
oB
�B
�B
�B
�B
!�B
#�B
,B
.B
.B
+B
�B
�B
hB
1B
  B	��B
  B
JB
hB
\B

=B

=B
DB
%B
  B	�B	�B	�B	�yB	�NB	�
B	��B	ǮB	ȴB	ŢB	ƨB	��B	��B	��B	��B	�B	�/B	�/B	�5B	�NB	�NB	�BB	�sB	�B	�B	�B	��B	�B	�B	�B	�B	�B	�fB	�NB	�BB	�/B	�)B	�)B	�/B	�BB	�HB	�NB	�NB	�ZB	�mB	�yB	�sB	�mB	�mB	�sB	�B
1B
JB
JB
JB
	7B
%B
B
B
B
B
B
%B
%B
B
B
B
%B
%B
%B
%B
%B
B
B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
+B
1B
1B
1B
1B
+B
%B
B
%B
%B
%B
%B
+B
%B
%B
B
B
B
B	��B	��B
B
B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
  B	��B	��B	��B	��B
B
B
  B	��B	��B
  B	��B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
	7B
	7B
	7B
1B
+B
%B
B
  B	��B
  B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
1B
1B
1B

=B
DB
DB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
PB
PB
VB
PB
PB
PB
PB
PB
PB
VB
VB
bB
�B
"�B
+B
1'B
9XB
@�B
G�B
K�B
R�B
W
B
_;B
dZB
hsB
l�B
p�B
s�B
w�B
{�B
� B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	ŢB	ŢB	ƨB	ŢB	ŢB	ĜB	ĜB	ĜB	ĜB	ŢB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ǮB	��B
�B
2-B
;dB
9XB
;dB
=qB
>wB
?}B
D�B
YB
{�B
��B
�'B
ŢB
ɺB
ŢB
�}B
�qB
��B
�TB
�fB
�yB
��B
��B
�B
�B
�B
�sBBO�B_;By�Bu�BaHB^5BI�B;dB;dB;dB49BPBB
��BBZBF�BD�BaHBgmBQ�B0!BoBB
�B
�jB
q�B
�B	�BB	�B	��B	�PB	|�B	`BB	K�B	49B	�B	%�B	 �B	�B	�B��B�HB�5B�B�B�B�B�B��B�'B�B�B�-BȴB�B�TB	�B	>wB	M�B	K�B	J�B	P�B	\)B	P�B	H�B	?}B	B�B	H�B	N�B	O�B	ZB	aHB	q�B	�B	�DB	��B	�B	��B	�B	�
B	�BB	��B	ŢB	ĜB	ȴB	�}B	��B	��B	ƨB	��B	��B	�NB	�fB	�B	�B	�B	�B	�B	��B	��B
  B
B
+B
B	��B	��B	��B	��B	�B	��B
B
B
1B
+B

=B
PB
PB
PB
\B
bB
bB
oB
�B
�B
�B
�B
"�B
#�B
,B
.B
/B
0!B
!�B
�B
{B
DB
B	��B	��B
JB
uB
hB
DB

=B
VB
1B
+B	��B	�B	�B	�B	�mB	�/B	��B	ȴB	ɺB	ǮB	ƨB	��B	��B	��B	��B	�B	�/B	�/B	�;B	�`B	�ZB	�BB	�sB	�B	�B	��B	��B	��B	��B	�B	�B	�B	�sB	�ZB	�NB	�5B	�/B	�/B	�5B	�HB	�NB	�TB	�TB	�ZB	�sB	�B	�yB	�sB	�sB	�sB	�B
1B
JB
PB
VB

=B
+B
B
B
B
B
%B
+B
+B
B
%B
%B
%B
%B
+B
+B
+B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B
  B	��B	��B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
B
%B
B
B
B
%B
+B
	7B
1B
1B
	7B
1B
1B
+B
%B
%B
%B
%B
+B
%B
+B
%B
B
B
B
  B	��B
B
B
B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B	��B	��B	��B	��B
B
B
B
B	��B
B	��B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
%B
B
	7B
	7B
DB
	7B
+B
1B
B
  B
  B
B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
	7B
	7B
	7B
	7B
DB
DB
JB
JB
JB
PB
PB
JB
JB
JB
PB
JB
PB
PB
PB
PB
VB
VB
VB
VB
PB
PB
VB
\B
VB
bB
�B
"�B
+B
1'B
9XB
@�B
G�B
K�B
R�B
XB
_;B
dZB
hsB
l�B
p�B
s�B
x�B
|�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250132012011312501320120113125013  AO  ARGQ                                                                        20111205112602  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112602  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125013  IP                  G�O�G�O�G�O�                