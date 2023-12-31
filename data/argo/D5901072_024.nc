CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:46Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143727  20190522121828  1728_5048_024                   2C  D   APEX                            2142                            040306                          846 @�f}�\��1   @�f~� @5:^5?|��c.I�^51   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D   D � D!  D!y�D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(fD(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=�fD>  D>y�D>��D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Dfy�Dg  Dg� Dh  Dh� Dh��Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dy��D� D�,�D�\�D��fD�3D�@ D�l�D�� D���D�,�D��3DǼ�D�� D��D�s3D�fD���D��D�` D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bn��Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ�fCS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��3C��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC�ٚC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC�ٚC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Dl�D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Dy�D��Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!l�D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'��D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=y�D=�3D>l�D>��D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKy�DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfl�Df�3Dgs3Dg�3Dhs3Dh��Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3Dy� D�	�D�&fD�VfD�� D���D�9�D�ffD�ɚD��fD�&fD�|�DǶfD�ٚD�3D�l�D� D��fD�fD�Y�D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ffA��A��RA�^5A�1'A��A�oA�JA�1A�JA�VA�
=A�1A�A�%A�%A�JA�bA���A�r�A�G�A�-A�  A���A�~�A�7LA�A���A�?}A���A��jA�|�A�jA�-A�VA���A��A��wA��A�z�A���A���A�dZA���A���A��RA�n�A��A�ĜA��A�jA�VA�M�A�G�A�7LA��A��/A���A�v�A�?}A��!A�|�A�t�A��`A� �A�hsA���A��;A� �A���A��A���A�`BA�p�A��A��!A�5?A�K�A��A�Q�A���A���A�"�A�1'A��yA��jA���A�=qA��;A�|�A��HA�+A��!A��9A��TA���A�Q�A�z�A�A�A�ȴA�
=A�jA� �A�E�A���A�?}A��
A�A��^A��`A�K�A~�`A{��Az{Ax�`Aw�Au�At��AqhsAn�9Al~�Ai�Ag�^AcƨA`�RA_`BA^��A]��A\ �AZjAYAW7LAV9XAV�AU��AU�ATZAR�AP1'AOhsAN�AM�ALjAJ��AI��AH�!AH  AG�7AE�ACG�AB{A@ĜA?�TA?p�A>E�A=�hA=`BA=+A<��A< �A;O�A:�yA:A�A9hsA7�A6�uA5��A4v�A3&�A2A1��A1�A05?A/��A/��A/`BA.��A-��A,��A,�9A,�A+x�A*n�A(M�A'oA&1A%dZA%?}A$r�A"�jA!x�A n�At�A9XA�#A�DA�-A�jAp�A�#A  A��A��Ar�A�A1A�A/A�+A�A-A
�jA	�TA�uA~�At�A�-A"�AI�A �HA �+@��P@�l�@���@��@� �@��H@��D@���@��@�h@���@�P@�=q@�?}@�D@�-@�\)@���@�n�@�v�@�S�@�
=@�\)@�bN@��@�V@ݩ�@�V@܋D@���@�C�@�x�@�x�@�+@Л�@��@���@�  @ˮ@���@���@ɑh@ɲ-@ȓu@�b@���@�ƨ@���@ȃ@�@˾w@�x�@��@�C�@�+@���@�-@�@�V@˕�@�S�@��@ʇ+@�$�@���@ɑh@ɲ-@�@�@ɑh@Ȭ@�1@��;@�ƨ@ǥ�@�|�@�"�@Ƨ�@�{@�@ũ�@�hs@���@�  @�t�@�C�@��@���@+@�=q@�-@��#@�`B@�7L@��/@�t�@���@��/@�1@��m@��@�o@�n�@�=q@��@��7@��/@���@�r�@�A�@��@���@�t�@�dZ@�C�@��!@�E�@�-@�J@���@��9@�bN@�9X@��@��P@�33@���@��#@�j@��@��F@�+@��+@��R@���@���@��@��@��h@��^@�?}@��@�r�@�9X@�1@��@�S�@�"�@���@��\@�-@��@��^@��h@�O�@��@���@���@���@�9X@�b@��;@��@�+@��!@�v�@�M�@��-@���@�Q�@�  @��@��m@���@�
=@���@�ff@���@�7L@�bN@�b@���@�;d@���@��@�~�@�{@���@���@�x�@�?}@���@���@�b@���@�S�@�@���@�ff@���@���@��@�z�@�b@���@�dZ@��H@��@��T@��^@�X@���@��@�j@��m@��P@�;d@���@���@��#@��^@�V@�A�@��w@��P@��P@�t�@�+@�@��@���@�~�@�n�@�M�@�5?@�@��T@���@��-@���@��h@�7L@��@��/@���@�Z@�(�@�1@�ƨ@�\)@�o@��@�ȴ@���@�v�@�^5@�E�@���@�@��-@���@���@�x�@�7L@���@�Ĝ@���@�A�@�(�@��@��@�ƨ@��@�ȴ@|9X@s"�@j^5@`�@V$�@O
=@G�@@b@9��@3�@.@)�@"~�@E�@~�@E�@�7@@	�71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ffA��A��RA�^5A�1'A��A�oA�JA�1A�JA�VA�
=A�1A�A�%A�%A�JA�bA���A�r�A�G�A�-A�  A���A�~�A�7LA�A���A�?}A���A��jA�|�A�jA�-A�VA���A��A��wA��A�z�A���A���A�dZA���A���A��RA�n�A��A�ĜA��A�jA�VA�M�A�G�A�7LA��A��/A���A�v�A�?}A��!A�|�A�t�A��`A� �A�hsA���A��;A� �A���A��A���A�`BA�p�A��A��!A�5?A�K�A��A�Q�A���A���A�"�A�1'A��yA��jA���A�=qA��;A�|�A��HA�+A��!A��9A��TA���A�Q�A�z�A�A�A�ȴA�
=A�jA� �A�E�A���A�?}A��
A�A��^A��`A�K�A~�`A{��Az{Ax�`Aw�Au�At��AqhsAn�9Al~�Ai�Ag�^AcƨA`�RA_`BA^��A]��A\ �AZjAYAW7LAV9XAV�AU��AU�ATZAR�AP1'AOhsAN�AM�ALjAJ��AI��AH�!AH  AG�7AE�ACG�AB{A@ĜA?�TA?p�A>E�A=�hA=`BA=+A<��A< �A;O�A:�yA:A�A9hsA7�A6�uA5��A4v�A3&�A2A1��A1�A05?A/��A/��A/`BA.��A-��A,��A,�9A,�A+x�A*n�A(M�A'oA&1A%dZA%?}A$r�A"�jA!x�A n�At�A9XA�#A�DA�-A�jAp�A�#A  A��A��Ar�A�A1A�A/A�+A�A-A
�jA	�TA�uA~�At�A�-A"�AI�A �HA �+@��P@�l�@���@��@� �@��H@��D@���@��@�h@���@�P@�=q@�?}@�D@�-@�\)@���@�n�@�v�@�S�@�
=@�\)@�bN@��@�V@ݩ�@�V@܋D@���@�C�@�x�@�x�@�+@Л�@��@���@�  @ˮ@���@���@ɑh@ɲ-@ȓu@�b@���@�ƨ@���@ȃ@�@˾w@�x�@��@�C�@�+@���@�-@�@�V@˕�@�S�@��@ʇ+@�$�@���@ɑh@ɲ-@�@�@ɑh@Ȭ@�1@��;@�ƨ@ǥ�@�|�@�"�@Ƨ�@�{@�@ũ�@�hs@���@�  @�t�@�C�@��@���@+@�=q@�-@��#@�`B@�7L@��/@�t�@���@��/@�1@��m@��@�o@�n�@�=q@��@��7@��/@���@�r�@�A�@��@���@�t�@�dZ@�C�@��!@�E�@�-@�J@���@��9@�bN@�9X@��@��P@�33@���@��#@�j@��@��F@�+@��+@��R@���@���@��@��@��h@��^@�?}@��@�r�@�9X@�1@��@�S�@�"�@���@��\@�-@��@��^@��h@�O�@��@���@���@���@�9X@�b@��;@��@�+@��!@�v�@�M�@��-@���@�Q�@�  @��@��m@���@�
=@���@�ff@���@�7L@�bN@�b@���@�;d@���@��@�~�@�{@���@���@�x�@�?}@���@���@�b@���@�S�@�@���@�ff@���@���@��@�z�@�b@���@�dZ@��H@��@��T@��^@�X@���@��@�j@��m@��P@�;d@���@���@��#@��^@�V@�A�@��w@��P@��P@�t�@�+@�@��@���@�~�@�n�@�M�@�5?@�@��T@���@��-@���@��h@�7L@��@��/@���@�Z@�(�@�1@�ƨ@�\)@�o@��@�ȴ@���@�v�@�^5@�E�@���@�@��-@���@���@�x�@�7L@���@�Ĝ@���@�A�@�(�@��@��@�ƨ@��@�ȴ@|9X@s"�@j^5@`�@V$�@O
=@G�@@b@9��@3�@.@)�@"~�@E�@~�@E�@�7@@	�71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB\BPBPBJBVBVB\B\B\BhBuB{B�B�B"�B/B7LB=qBVB��B�9B�FBÖB��BDB9XBH�B[#B[#B[#B\)B`BB^5B`BBffBo�Bv�B}�B�PB��BƨB��B��B��BɺBƨBBŢBȴBɺBɺB��B��B��B��B��BƨBÖB��B�qBŢBǮB��BȴBĜB�}B�-B�-B�'B�B�!B�B��B��B��B��Bz�Bl�B]/BW
BL�BA�B9XB)�B�BuBPB  B�B�/B��B��BƨB�qB�\B}�By�Bm�BO�B0!B{B1B
��B
�B
�/B
ÖB
�RB
��B
�DB
z�B
_;B
VB
<jB
.B
#�B
�B
JB
  B	�mB	��B	�dB	�B	��B	�+B	jB	dZB	dZB	YB	[#B	H�B	A�B	:^B	0!B	.B	-B	(�B	#�B	�B	�B	uB	PB	+B	B	JB	  B��B��B	B�B��B��B�HB�HB�`B�ZB�BB�NB�TB�fB�B�B�B�yB�#B��BŢBÖB�wB�!B�B�!B�B�B�B�B�BÖB�B��B�B�'B�B��B��B��B�uB�uB�uB��B�\B�\B�DB�1B�B�B�B�B�B{�Bx�Bw�Bs�Bp�Bl�BjBjBl�Bl�Bw�BjBjBw�Bo�BXB{�B}�Bm�B^5BVBiyBe`Bl�BbNB`BB\)BaHBaHBT�BVBP�BO�BK�BR�BP�BT�BW
B[#B`BBcTBcTBbNBk�Bo�BjBy�B|�Bu�Br�Bp�Bo�Bn�Bp�Bn�BbNBaHBYBbNBYB\)Be`BgmBn�Bz�B�DB��B��B��B��B��B��B�'BǮB�B�B��B��B��B	B	B	+B	VB	VB	hB	{B	�B	�B	�B	�B	�B	 �B	"�B	#�B	%�B	'�B	(�B	,B	-B	.B	0!B	33B	6FB	7LB	8RB	=qB	>wB	@�B	@�B	@�B	@�B	B�B	C�B	C�B	D�B	F�B	F�B	G�B	M�B	R�B	S�B	W
B	YB	[#B	]/B	_;B	aHB	dZB	ffB	jB	k�B	l�B	n�B	o�B	s�B	t�B	t�B	u�B	y�B	|�B	}�B	~�B	�B	�%B	�7B	�7B	�=B	�JB	�JB	�DB	�VB	�VB	�VB	�VB	�PB	�JB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�9B	�?B	�?B	�?B	�FB	�FB	�FB	�LB	�LB	�RB	�RB	�XB	�^B	�dB	�jB	�jB	�qB	��B	B	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�)B	�)B	�#B	�)B	�;B	�;B	�5B	�BB	�BB	�BB	�HB	�ZB	�`B	�ZB	�ZB	�`B	�TB	�TB	�fB	�`B	�`B	�ZB	�ZB	�`B	�fB	�sB	�yB	�yB	�B	�sB	�sB	�sB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B
  B
B
B
B
B
B
+B
bB
�B
�B
"�B
-B
8RB
=qB
D�B
J�B
O�B
T�B
ZB
_;B
e`B
jB
l�B
p�B
u�B
x�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BhB\B\BPBVBVB\B\B\BhBuB{B�B�B"�B/B7LB=qBXB��B�9B�LBĜB�BJB:^BJ�B]/B\)B\)B]/B`BB_;BaHBgmBp�Bw�B~�B�PB��BƨB�B��B��B��BȴBŢBƨB��B��B��B��B��B��B��B��BǮBĜBB��B��B��B��B��BǮBƨB�FB�FB�dB�LB�'B�3B�-B��B��B��B}�Bo�B^5BYBQ�BD�B=qB0!B%�B�BhB+B�B�BB�B��B��BƨB��B�B}�Bt�B]/B9XB�B\B
��B
�B
�NB
ȴB
�jB
�B
�\B
�B
cTB
]/B
@�B
1'B
'�B
�B
\B
1B	�B	��B	ÖB	�'B	�B	�bB	n�B	gmB	ffB	_;B	`BB	M�B	G�B	=qB	1'B	/B	/B	-B	+B	�B	�B	�B	oB	DB	1B	\B	B��B��B	1B��B��B��B�TB�NB�sB�fB�HB�TB�ZB�sB�B�B�B�B�BB��BȴBǮBB�9B�B�-B�-B�B�B�B�-BƨB�!B�B�'B�3B�B��B��B��B��B�{B��B��B�uB�oB�VB�DB�%B�+B�B�B�B�B}�B{�Bv�Bt�Bp�Bm�Bl�Bm�Bn�By�Bm�Bm�By�Br�B]/B~�B�Bo�BaHBZBjBgmBm�Be`BaHB^5BcTBe`BZBXBS�BP�BM�BT�BR�BVB[#B_;BaHBdZBcTBaHBl�Bt�BiyB}�B�Bv�Bs�Bq�Bp�Bo�Bs�Bt�BffBe`BZBgmB[#B]/BffBiyBo�Bz�B�JB��B��B��B��B��B��B�!BŢB�
B�B��B��B��B	B	B		7B	\B	\B	oB	{B	�B	�B	�B	�B	�B	!�B	$�B	$�B	%�B	'�B	(�B	-B	.B	/B	1'B	49B	6FB	8RB	9XB	?}B	?}B	A�B	@�B	@�B	A�B	B�B	C�B	D�B	E�B	G�B	G�B	J�B	P�B	T�B	VB	W
B	ZB	[#B	^5B	`BB	bNB	e`B	gmB	k�B	l�B	m�B	o�B	o�B	t�B	t�B	t�B	v�B	z�B	|�B	}�B	� B	�B	�+B	�7B	�=B	�DB	�PB	�PB	�JB	�bB	�\B	�\B	�\B	�VB	�JB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�3B	�?B	�FB	�?B	�FB	�LB	�FB	�FB	�RB	�LB	�RB	�XB	�^B	�dB	�jB	�qB	�qB	�wB	B	ÖB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�/B	�/B	�)B	�/B	�BB	�BB	�;B	�HB	�HB	�HB	�NB	�`B	�fB	�`B	�`B	�fB	�ZB	�ZB	�fB	�`B	�fB	�`B	�ZB	�`B	�mB	�yB	�B	�B	�B	�sB	�sB	�yB	�yB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
  B
  B
B
B
B
B
B
B
+B
bB
�B
�B
"�B
-B
8RB
=qB
D�B
J�B
O�B
T�B
ZB
_;B
e`B
jB
l�B
p�B
u�B
x�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452012012011014520120120110145201  AO  ARGQ                                                                        20111130143727  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143727  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145201  IP                  G�O�G�O�G�O�                