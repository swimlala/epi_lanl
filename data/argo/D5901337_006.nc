CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:25Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �`   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �h   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112403  20190522121836  1901_5055_006                   2C  D   APEX                            2140                            040306                          846 @�8Rw���1   @�8S1M� @0�p��
=�c0 ě��1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bo��BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQfDQ� DRfDR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dy� D�3D�,�D�l�D��3D�3D�<�D�\�D�� D�  D�9�D�� D��3D���D�)�D�s3D��D��3D��D�VfD�ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�ff@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'��B/33B733B?33BG33BO33BW33B_33Bg33Bn��Bw��B33B���B���B���B���B���B���B���B���B���B���B�ffB���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5�fC7�fC9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU�fCW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��3C��fC��fC��fC��3C��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"��D#y�D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP��DQs3DQ��DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3Dy�3D���D�&fD�ffD���D��D�6fD�VfD���D���D�33D���DǼ�D��fD�#3D�l�D�fD���D�fD�P D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AٍPAٍPAُ\AٍPAٍPAُ\AّhAٓuAٙ�Aٙ�Aٕ�Aٕ�Aٗ�Aٕ�Aٗ�Aٛ�Aٕ�AًDA�x�A�  A�C�A���A�&�A԰!A�^5A��A�VA�I�A���A�^5Aͩ�A��A��A�$�A���A��A��A�?}A��#A�Q�A�9XA�VA�-A��`A�(�A��A�(�A��jA���A��-A��A��A���A��A���A�bNA���A�VA�$�A���A�VA���A���A�oA���A�A�Q�A��A�(�A�t�A�33A�$�A�v�A�bNA�ȴA���A�  A��A�l�A�{A�oA��+A�~�A��\A��`A�bA�x�A���A���A�A�x�A���A��FA�K�A}`BAy�Av�Au�Ar�`Ap  Ak|�Af1'Aa�7A^{AZbNAW�^AW�AVQ�AS+AOAMt�AK�-AH�yAF$�ADI�AC\)AA�AA;dA@^5A?��A=�A<ȴA;��A9A8ffA7�PA6�\A3��A1G�A0ZA/�;A.�A.5?A-�A-t�A*�HA'�A&�+A%p�A$ �A#��A#dZA#�hA#\)A"�DA!��A ~�A�AA�An�A|�A��A �AAZAI�A�!AI�A�TA�PAdZA��A~�AM�AA�A��A/A�/Az�AZAQ�A �A�A\)A�A
=A��A�A��A�A�A-A�PA33A��AȴA�uA�Al�A7LA�A%AjA  A�hAoA��A��AZA��AO�A�A
�A
��A
bNA
A�A	��A	�A��AZA�TA��A�7A�A��An�A�Ax�AoA�!AjA1'A�#A�A�!A=qAAC�A ��A ffA 1@��w@��!@���@�G�@��j@� �@��P@���@�J@�G�@�;d@���@��H@��\@��-@���@�1@��@��@���@�O�@��@�(�@@���@��H@��@@�G�@���@웦@��;@�S�@��H@�J@�O�@蛦@�  @�w@�P@�@�V@噚@�j@��
@�S�@�
=@�n�@��@��@��@�A�@�  @߾w@��@�n�@��#@�`B@�V@ܼj@�j@��@���@�K�@��y@���@ڧ�@�ff@�@١�@�X@�%@�+@�V@�V@�5?@��@��
@ӍP@�@ҏ\@�{@ѩ�@��@��@��@��@��@͑h@̬@˶F@ˮ@ˮ@�l�@�
=@��@�v�@ɩ�@��@�~�@�E�@�x�@��@��`@�r�@���@Õ�@î@�C�@���@+@�V@�J@���@��@�G�@�X@�O�@�`B@�r�@���@��7@��@�Q�@�b@��F@�C�@�ȴ@���@�=q@��@��@��@���@��D@�I�@�1@�t�@�+@��H@�^5@��-@�7L@�%@���@�bN@��m@�t�@�S�@�+@��@���@�n�@��#@�&�@���@�Q�@��
@�t�@��H@�V@���@�G�@��u@�A�@�  @�|�@��@���@��u@��P@���@��R@�X@���@��H@���@�@�&�@�Q�@�C�@�5?@�p�@�7L@��@�/@�&�@�9X@���@�K�@�ȴ@�ff@�{@��-@�%@���@�Ĝ@�Ĝ@��@��u@�r�@�I�@��@���@�dZ@�"�@�"�@���@��@��R@�ff@�$�@�J@��T@���@�hs@�7L@��@��/@���@�A�@��w@��P@�K�@�
=@��\@�J@���@�O�@�&�@���@��j@��D@�A�@���@�o@�E�@��@��@���@�hs@�/@��@��9@�Ĝ@�Ĝ@�Ĝ@��9@��u@�A�@�b@��P@�t�@�K�@�
=@��@��!@�v�@��@���@��j@���@��@��/@t�@i��@a7L@Yhs@PĜ@G�P@@Q�@:��@5�@/K�@)�7@$��@�@o@K�@��@|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AٍPAٍPAُ\AٍPAٍPAُ\AّhAٓuAٙ�Aٙ�Aٕ�Aٕ�Aٗ�Aٕ�Aٗ�Aٛ�Aٕ�AًDA�x�A�  A�C�A���A�&�A԰!A�^5A��A�VA�I�A���A�^5Aͩ�A��A��A�$�A���A��A��A�?}A��#A�Q�A�9XA�VA�-A��`A�(�A��A�(�A��jA���A��-A��A��A���A��A���A�bNA���A�VA�$�A���A�VA���A���A�oA���A�A�Q�A��A�(�A�t�A�33A�$�A�v�A�bNA�ȴA���A�  A��A�l�A�{A�oA��+A�~�A��\A��`A�bA�x�A���A���A�A�x�A���A��FA�K�A}`BAy�Av�Au�Ar�`Ap  Ak|�Af1'Aa�7A^{AZbNAW�^AW�AVQ�AS+AOAMt�AK�-AH�yAF$�ADI�AC\)AA�AA;dA@^5A?��A=�A<ȴA;��A9A8ffA7�PA6�\A3��A1G�A0ZA/�;A.�A.5?A-�A-t�A*�HA'�A&�+A%p�A$ �A#��A#dZA#�hA#\)A"�DA!��A ~�A�AA�An�A|�A��A �AAZAI�A�!AI�A�TA�PAdZA��A~�AM�AA�A��A/A�/Az�AZAQ�A �A�A\)A�A
=A��A�A��A�A�A-A�PA33A��AȴA�uA�Al�A7LA�A%AjA  A�hAoA��A��AZA��AO�A�A
�A
��A
bNA
A�A	��A	�A��AZA�TA��A�7A�A��An�A�Ax�AoA�!AjA1'A�#A�A�!A=qAAC�A ��A ffA 1@��w@��!@���@�G�@��j@� �@��P@���@�J@�G�@�;d@���@��H@��\@��-@���@�1@��@��@���@�O�@��@�(�@@���@��H@��@@�G�@���@웦@��;@�S�@��H@�J@�O�@蛦@�  @�w@�P@�@�V@噚@�j@��
@�S�@�
=@�n�@��@��@��@�A�@�  @߾w@��@�n�@��#@�`B@�V@ܼj@�j@��@���@�K�@��y@���@ڧ�@�ff@�@١�@�X@�%@�+@�V@�V@�5?@��@��
@ӍP@�@ҏ\@�{@ѩ�@��@��@��@��@��@͑h@̬@˶F@ˮ@ˮ@�l�@�
=@��@�v�@ɩ�@��@�~�@�E�@�x�@��@��`@�r�@���@Õ�@î@�C�@���@+@�V@�J@���@��@�G�@�X@�O�@�`B@�r�@���@��7@��@�Q�@�b@��F@�C�@�ȴ@���@�=q@��@��@��@���@��D@�I�@�1@�t�@�+@��H@�^5@��-@�7L@�%@���@�bN@��m@�t�@�S�@�+@��@���@�n�@��#@�&�@���@�Q�@��
@�t�@��H@�V@���@�G�@��u@�A�@�  @�|�@��@���@��u@��P@���@��R@�X@���@��H@���@�@�&�@�Q�@�C�@�5?@�p�@�7L@��@�/@�&�@�9X@���@�K�@�ȴ@�ff@�{@��-@�%@���@�Ĝ@�Ĝ@��@��u@�r�@�I�@��@���@�dZ@�"�@�"�@���@��@��R@�ff@�$�@�J@��T@���@�hs@�7L@��@��/@���@�A�@��w@��P@�K�@�
=@��\@�J@���@�O�@�&�@���@��j@��D@�A�@���@�o@�E�@��@��@���@�hs@�/@��@��9@�Ĝ@�Ĝ@�Ĝ@��9@��u@�A�@�b@��P@�t�@�K�@�
=@��@��!@�v�@��@���@��j@���@��@��/@t�@i��@a7L@Yhs@PĜ@G�P@@Q�@:��@5�@/K�@)�7@$��@�@o@K�@��@|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ĜB
ĜB
ĜB
ŢB
ŢB
ĜB
ŢB
ŢB
ĜB
ĜB
ĜB
ĜB
ÖB
��B
�dB
�LB
�qB
ÖB
�wB
�RB
�dB
�}B
�mB
�B
�B
�B
�sB
�B
��B
=B�B=qBR�B[#BbNBy�B�B�BB�BG�B:^BS�BhsBk�BaHBS�BP�BT�BD�B33B$�B��Bu�B!�B
�BB1'B0!B'�B�B�B\BDBuB�oB�B�wB�BoB��B�^B��B�qB��B{�BjB6FB
�ZB
q�B
K�B
"�B
	7B	�B	�mB	�B	�FB	��B	�JB	~�B	m�B	dZB	VB	B�B	&�B	B�B�NB�B��B��B��B�NB�HB�HB�`B�B�B�sB�B	  B	B	B	JB	&�B	,B	)�B	5?B	;dB	<jB	<jB	7LB	1'B	.B	1'B	1'B	49B	5?B	8RB	$�B	{B	uB	�B	�B	#�B	,B	7LB	A�B	D�B	J�B	L�B	M�B	O�B	T�B	_;B	bNB	^5B	r�B	�B	�PB	�uB	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�^B	�jB	�wB	��B	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�NB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�mB	�`B	�ZB	�TB	�ZB	�`B	�`B	�fB	�fB	�ZB	�fB	�fB	�`B	�ZB	�TB	�mB	�mB	�sB	�sB	�yB	�yB	�mB	�TB	�BB	�BB	�/B	�/B	�)B	�)B	�;B	�;B	�NB	�HB	�ZB	�`B	�`B	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
  B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
%B
B
B
B
B
B
B
B
+B

=B
DB
DB
JB
JB
PB
VB
\B
VB
\B
\B
\B
bB
bB
hB
hB
uB
�B
 �B
(�B
0!B
9XB
=qB
A�B
I�B
P�B
W
B
[#B
aHB
cTB
gmB
n�B
r�B
u�B
x�B
}�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ĜB
ĜB
ĜB
ŢB
ŢB
ĜB
ŢB
ŢB
ĜB
ĜB
ĜB
ƨB
ŢB
ĜB
��B
�XB
�wB
ŢB
��B
�dB
��B
ȴB
�yB
�B
�B
��B
�B
�BBVB �BD�BR�B[#BbNB}�B�B�BI�BM�B>wBT�Bk�Bp�BffBYBT�B[#BH�B5?B+BDB�B-B
�B1B7LB2-B+B$�B�BoBbB\B�hB�B�jB�B �B�B��B��BB�B�Bt�BK�B
��B
{�B
\)B
0!B
oB	��B	�B	�;B	��B	��B	��B	�7B	p�B	jB	^5B	O�B	5?B	oB��B�B�BB��B��B�B�B�`B�`B�B��B�B�B��B	B	%B	%B	hB	)�B	/B	0!B	9XB	>wB	@�B	D�B	?}B	49B	0!B	49B	33B	5?B	6FB	>wB	,B	�B	�B	�B	�B	$�B	,B	8RB	C�B	F�B	M�B	N�B	P�B	P�B	VB	bNB	e`B	_;B	r�B	�B	�VB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�dB	�wB	�}B	B	ƨB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�B	�B	�)B	�5B	�;B	�HB	�TB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B	��B
  B
B
B
B
  B
B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�fB	�`B	�ZB	�`B	�fB	�fB	�sB	�sB	�`B	�fB	�mB	�mB	�fB	�TB	�mB	�mB	�yB	�yB	�B	�B	�B	�fB	�HB	�HB	�5B	�5B	�/B	�/B	�BB	�;B	�TB	�NB	�`B	�fB	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	�B	��B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B
	7B
+B
B
B
%B
B
B
%B
B
+B

=B
DB
DB
JB
PB
PB
\B
\B
VB
bB
\B
\B
hB
hB
oB
hB
{B
�B
 �B
(�B
1'B
:^B
=qB
A�B
J�B
P�B
W
B
[#B
aHB
cTB
gmB
n�B
r�B
v�B
y�B
}�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��
<e`B<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<�1<���<#�
<�o<T��<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<D��<e`B<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250092012011312500920120113125009  AO  ARGQ                                                                        20111205112403  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112403  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125009  IP                  G�O�G�O�G�O�                