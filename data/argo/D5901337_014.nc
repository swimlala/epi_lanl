CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:27Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z           9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�        C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�        M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112518  20190522121836  1901_5055_014                   2C  D   APEX                            2140                            040306                          846 @�L8�5@
1   @�L<��@/��^5?}�c�9XbN1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8��B?33BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�ffB���B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C{�fC~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD fD �fD!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dyl�D�3D�FfD��3D��fD��fD�&fD�� D��fD���D�fD�VfDǳ3D�fD�0 D�|�D� D�� D�	�D�9�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�ff@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B8  B>ffBG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B�  B�33B���B�ffB���B���B�ffB���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�B���Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	�fC�fC��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9�fC;��C=��C?��CA��CC��CE��CG��CI��CK�3CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg�3Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{�3C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Dl�D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Dy�D��D y�D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dy` D���D�@ D�|�D�� D�� D�  D�y�D�� D��fD� D�P DǬ�D�  D�)�D�vfDਗ਼D�ɚD�3D�3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���Aٺ^AٸRAٰ!Aٰ!AٮA٬A٬A٬A٬A٬A٬AٮA٬AٮAٮAٮA٣�Aٗ�A�%AؑhAף�A�v�A�x�A���A�-Aҥ�Aѣ�AБhAϋDA�^5A���A��yAơ�A�&�A�O�A�p�A��FA��A��TA�XA��9A���A��A�hsA�p�A�"�A��A���A���A��^A��A�ĜA���A�x�A��jA�XA���A���A��A�ĜA��^A���A�-A�5?A��^A��A�E�A���A���A�VA���A�C�A��A���A�r�A��A�/A{\)Aw�As�PApQ�Ao��Am��AjAf�Ab5?AY�7AUx�AS�#AP$�AJȴAF��AB~�A@{A=��A<VA:ĜA:1A9C�A8�HA7�7A5��A3�mA2jA1?}A/�7A-��A,�A,I�A+��A+��A)�wA(ȴA(=qA'�mA'�hA&n�A%XA%&�A%"�A$�!A$$�A"�yA!+A��AAt�A�AVA�`AVA�7A�RA�A�A�!A�FA��A��A��A=qAM�AJA��A~�AAz�A��A{A5?A�PAS�A�AbAG�AE�A�#A�A��A/A-A�9A�DAI�A�uAz�A��A�A
��A
�uA
~�A
 �A	K�A�HA �A�PA&�A��A��AhsA�FA�wA$�A�!A	oA+A�AI�A�mA�A(�A�
Al�AO�A/AA
jA	�A	�FA	S�A	�AE�A��A�A&�A�A�AA��AS�AAZA=qA$�A�mAG�AȴAE�A1A^5A��A�AA��A�yA��Av�A{A ��A z�A M�A ffA �DA r�A 1'@���@��j@�l�@��H@�+@�@�9X@�@�@�-@�@�@�@�/@�l�@�C�@�+@�R@��@�@�j@�@��@�r�@�n�@噚@��@�x�@���@�|�@�@��@�
=@��m@�5?@�/@ج@׍P@�`B@�&�@�X@�J@�M�@�5?@��T@թ�@���@ՙ�@ա�@�/@���@�j@�ƨ@�dZ@�o@��H@�J@щ7@��@�9X@Ͼw@ϥ�@���@͑h@�&�@�Ĝ@�z�@� �@��@���@ˍP@�33@ʸR@��T@ɑh@�7L@�Q�@ǶF@�t�@�K�@Ɨ�@š�@�X@���@�(�@�\)@���@�v�@�5?@��@���@�O�@�/@�Ĝ@��@��@��R@�-@�p�@�&�@�j@�|�@�E�@��7@��@�r�@�1@��@���@�S�@�K�@��+@�=q@��@�A�@���@�dZ@�
=@���@���@��@��@���@�j@�ƨ@�33@��H@��R@��!@��+@�E�@���@�x�@���@�z�@�1@��m@��@���@���@�S�@�"�@���@��\@�n�@��@�hs@�%@��@�1'@���@���@��w@��F@���@��P@�t�@�S�@�o@�ȴ@�~�@��@�/@��j@�j@��;@��P@�33@�o@��H@���@�E�@�J@���@���@���@��9@�r�@�A�@�  @���@��@��
@�S�@���@��!@�J@���@�G�@��@���@��j@�I�@��@���@��@��F@��@�;d@��@��R@�-@��@���@�O�@�&�@���@��@�@��+@�{@��@��^@���@�`B@�V@��@��j@�1@�ƨ@��F@���@�l�@�C�@��H@���@���@�V@�-@�J@�@�O�@���@�9X@�1@��w@��H@���@��+@��\@���@��!@���@���@��7@�G�@�V@���@��D@��@�b@�S�@�@��@�ȴ@�/@�O�@�A�@uV@l�j@a�7@Y�@R^5@M�@E?}@@b@8�`@0�9@*-@$�@��@n�@
=@�F@1'11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���Aٺ^AٸRAٰ!Aٰ!AٮA٬A٬A٬A٬A٬A٬AٮA٬AٮAٮAٮA٣�Aٗ�A�%AؑhAף�A�v�A�x�A���A�-Aҥ�Aѣ�AБhAϋDA�^5A���A��yAơ�A�&�A�O�A�p�A��FA��A��TA�XA��9A���A��A�hsA�p�A�"�A��A���A���A��^A��A�ĜA���A�x�A��jA�XA���A���A��A�ĜA��^A���A�-A�5?A��^A��A�E�A���A���A�VA���A�C�A��A���A�r�A��A�/A{\)Aw�As�PApQ�Ao��Am��AjAf�Ab5?AY�7AUx�AS�#AP$�AJȴAF��AB~�A@{A=��A<VA:ĜA:1A9C�A8�HA7�7A5��A3�mA2jA1?}A/�7A-��A,�A,I�A+��A+��A)�wA(ȴA(=qA'�mA'�hA&n�A%XA%&�A%"�A$�!A$$�A"�yA!+A��AAt�A�AVA�`AVA�7A�RA�A�A�!A�FA��A��A��A=qAM�AJA��A~�AAz�A��A{A5?A�PAS�A�AbAG�AE�A�#A�A��A/A-A�9A�DAI�A�uAz�A��A�A
��A
�uA
~�A
 �A	K�A�HA �A�PA&�A��A��AhsA�FA�wA$�A�!A	oA+A�AI�A�mA�A(�A�
Al�AO�A/AA
jA	�A	�FA	S�A	�AE�A��A�A&�A�A�AA��AS�AAZA=qA$�A�mAG�AȴAE�A1A^5A��A�AA��A�yA��Av�A{A ��A z�A M�A ffA �DA r�A 1'@���@��j@�l�@��H@�+@�@�9X@�@�@�-@�@�@�@�/@�l�@�C�@�+@�R@��@�@�j@�@��@�r�@�n�@噚@��@�x�@���@�|�@�@��@�
=@��m@�5?@�/@ج@׍P@�`B@�&�@�X@�J@�M�@�5?@��T@թ�@���@ՙ�@ա�@�/@���@�j@�ƨ@�dZ@�o@��H@�J@щ7@��@�9X@Ͼw@ϥ�@���@͑h@�&�@�Ĝ@�z�@� �@��@���@ˍP@�33@ʸR@��T@ɑh@�7L@�Q�@ǶF@�t�@�K�@Ɨ�@š�@�X@���@�(�@�\)@���@�v�@�5?@��@���@�O�@�/@�Ĝ@��@��@��R@�-@�p�@�&�@�j@�|�@�E�@��7@��@�r�@�1@��@���@�S�@�K�@��+@�=q@��@�A�@���@�dZ@�
=@���@���@��@��@���@�j@�ƨ@�33@��H@��R@��!@��+@�E�@���@�x�@���@�z�@�1@��m@��@���@���@�S�@�"�@���@��\@�n�@��@�hs@�%@��@�1'@���@���@��w@��F@���@��P@�t�@�S�@�o@�ȴ@�~�@��@�/@��j@�j@��;@��P@�33@�o@��H@���@�E�@�J@���@���@���@��9@�r�@�A�@�  @���@��@��
@�S�@���@��!@�J@���@�G�@��@���@��j@�I�@��@���@��@��F@��@�;d@��@��R@�-@��@���@�O�@�&�@���@��@�@��+@�{@��@��^@���@�`B@�V@��@��j@�1@�ƨ@��F@���@�l�@�C�@��H@���@���@�V@�-@�J@�@�O�@���@�9X@�1@��w@��H@���@��+@��\@���@��!@���@���@��7@�G�@�V@���@��D@��@�b@�S�@�@��@�ȴ@�/@�O�@�A�@uV@l�j@a�7@Y�@R^5@M�@E?}@@b@8�`@0�9@*-@$�@��@n�@
=@�F@1'11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	,B	,B	,B	,B	,B	,B	-B	-B	.B	.B	.B	.B	/B	/B	/B	/B	/B	0!B	49B	6FB	`BB	�B	ŢB
�DB
�jB
�
B
�ZB
�HB
�mB
�B
�B
��BB
=BoB	7BBB
�5B
ŢB
��B%B�BH�Bn�B�+B��B�bB��B�B�BJB>wB?}B9XB1'BhBB�VB�B��B��B�#B��B?}B�B1B(�B33B�B�ZB�%B8RBB
ȴB
o�B
�B	�yB	�}B	��B	z�B	dZB	S�B	M�B	>wB	(�B	oB��B��BÖB�jB�B��B�bB��B��B��B�^B�}B��B��B��BĜB��B��B��B�#B�fB�;B�NB�B��B	B		7B	PB	\B	\B	VB	bB	�B	"�B	.B	;dB	H�B	G�B	I�B	J�B	E�B	F�B	G�B	G�B	G�B	K�B	Q�B	T�B	cTB	ffB	ffB	o�B	y�B	�B	�JB	��B	��B	�B	�B	�B	��B	��B	��B	��B	�DB	�DB	�VB	��B	��B	��B	��B	��B	��B	�'B	�9B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�3B	�qB	ƨB	��B	�B
\B
PB
\B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
bB
bB
hB
bB
JB
B
B
B
B
B
B
B
B
  B
B
\B
�B
�B
�B
�B
�B
�B
�B
�B
uB
oB
oB
�B
�B
�B
VB	��B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�sB	�BB	�)B	�B	�B	��B	��B	��B	�
B	�/B	�BB	�BB	�BB	�TB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�mB	�fB	�B	�B	�B	�B	�B	�sB	�fB	�fB	�sB	�yB	�yB	�yB	�B	�B	�yB	�sB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
  B
B
B
B
B
B
B
B
B
%B
+B
%B
+B
1B
	7B
1B
1B
1B
	7B
DB
DB
DB
DB
DB
DB
DB
JB
PB
PB
VB
VB
VB
VB
\B
\B
bB
bB
bB
bB
bB
\B
bB
bB
\B
\B
\B
\B
bB
hB
uB
{B
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
$�B
+B
0!B
0!B
;dB
F�B
K�B
N�B
T�B
YB
_;B
e`B
jB
l�B
q�B
u�B
x�B
{�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	,B	,B	,B	,B	,B	,B	-B	-B	.B	.B	.B	.B	/B	/B	/B	/B	/B	0!B	49B	8RB	bNB	�B	ɺB
�VB
�wB
�B
�fB
�`B
�B
�B
��BBDBhB�BVBBB
�fB
ɺBBDB"�BL�Bv�B�PB�-B�hBÖB�B�BDB?}B@�B:^B5?B�BȴB�oB�%B�B��B�;B��BH�B�B1B+B7LB�B�B�bB>wB
=B
�
B
�+B
-B	��B	��B	��B	�%B	m�B	W
B	S�B	I�B	33B	�B	PB�/BȴBÖB�9B��B��B��B�B��B�wB��BBBŢB��B��B�B�
B�HB�B�NB�ZB�B��B	1B	PB	\B	bB	bB	hB	uB	�B	"�B	/B	=qB	K�B	L�B	M�B	N�B	G�B	G�B	G�B	H�B	I�B	M�B	S�B	S�B	cTB	gmB	iyB	o�B	z�B	�B	�DB	��B	��B	�B	�3B	�-B	�B	��B	��B	��B	�PB	�JB	�bB	��B	��B	��B	��B	��B	��B	�'B	�LB	�B	��B	��B	��B	�B	�'B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�-B	�dB	ŢB	ȴB	�B
bB
VB
\B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
bB
bB
oB
oB
hB
B
B
B
B
B
B
B
B
B
B
VB
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
oB
�B
�B
�B
�B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�NB	�/B	�B	�B	�B	��B	��B	�B	�/B	�BB	�HB	�BB	�TB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�mB	�yB	�B	�B	�B	�B	�yB	�mB	�mB	�yB	�yB	�yB	�yB	�B	�B	�B	�yB	�sB	�sB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
	7B

=B

=B
	7B
	7B
	7B
DB
DB
JB
JB
DB
JB
JB
PB
PB
PB
VB
VB
\B
VB
\B
bB
bB
bB
hB
hB
hB
hB
hB
hB
hB
bB
\B
\B
bB
hB
uB
�B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
$�B
,B
0!B
0!B
;dB
F�B
K�B
N�B
T�B
ZB
_;B
e`B
jB
l�B
q�B
u�B
x�B
{�B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<e`B<�j<�C�<49X<e`B<T��<49X<#�
<#�
<#�
<49X<#�
<T��<�j<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250122012011312501220120113125012  AO  ARGQ                                                                        20111205112518  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112518  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125012  IP                  G�O�G�O�G�O�                