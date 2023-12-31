CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:57Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Kl   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mh   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  UP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �\   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �l   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135630  20190522121825  1727_5046_024                   2C  D   APEX                            2143                            040306                          846 @�4��u�1   @�4��H@@7ix����c�G�z�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&y�D'  D'� D(  D(y�D(��D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dj��Dky�Dk��Dl� DmfDm� Dn  Dn�fDofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy�D�6fD�ffD���D��3D�3D�ffD���D�� D�&fD�l�D�� D�� D�3D�c3Dڳ3D��3D�3D�` D�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'��B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw��B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C�fC��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW�fCY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Dl�D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D��Ds3D�3Ds3D�3Ds3D�3Dl�D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&l�D&�3D's3D'�3D(l�D(��D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De��Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj��Dkl�Dk��Dls3Dl��Dms3Dm�3Dny�Dn��Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Dy  D�0 D�` D��fD���D��D�` D��3D�ɚD�  D�ffD���D�ٚD��D�\�Dڬ�D���D��D�Y�D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��`Aа!AХ�AЙ�AГuAЉ7AЇ+AЅAЃAЃAЁA�~�A�~�A�|�A�|�A�|�A�z�A�z�A�|�AЁAЋDAЏ\AЏ\A�jA��AͰ!A˛�A��A�I�Aȧ�A��A��#A�O�A�33A�x�A���A��A���A���A�A�?}A�E�A�I�A�t�A�ȴA�\)A�&�A�x�A��RA�%A��DA��A�=qA�O�A��A�x�A�C�A�JA��mA��jA��RA��FA���A��A��FA���A�^5A���A���A��A��RA�"�A���A���A�p�A��`A��+A�bA�
=A�VA���A�r�A�M�A�JA�M�A��;A�+A��A�-A���A��^A���A��A���A��#A�~�A��TA�`BA���A�ĜA�I�A���A��A�O�A��
A�r�A�bA���A��TA���A�`BA�r�A��A�A�A~��A}��A}t�A}&�A|�!A{��Ay33AwO�AvffAu33AshsAr��Ar�jArE�ArbAq�wAq7LAp��Ap�/Ao��An  AlbAi�PAh=qAf�AfM�Ad��Ab9XAaA`(�A^�9A\��AZ��AY+AWK�AU��AT�/AS�-AR��ARVAQ�#AQ��AQ\)APE�AO��AN9XAL9XAJȴAH1'AF��AEp�AE�AD��AC��AC�AC33AB�\AA�#AA�^AA�A@�A@ �A>�jA=�A;G�A9��A8-A6��A4Q�A2ȴA1�FA1"�A0 �A/A/�hA/C�A.n�A.$�A-�A,�A+ƨA*�A)x�A)C�A'A&jA%��A%\)A$�RA$$�A!A!K�A ��A �A�7A"�A��AK�A(�A��A%A5?A�A|�A�!A��A7LAZA-A�;A�A1'A�^A
=A�7A�`AQ�AO�AJA�7AVA^5Ap�A
�!A	G�AAr�AI�A�-A"�A�RAS�A1'A�A @��@���@�9X@��y@���@�1@�l�@�O�@���@�+@홚@�@�b@��@��@���@��@���@�l�@�+@�o@�ff@�bN@�ȴ@��@ݙ�@ܬ@ۍP@�v�@�p�@�Ĝ@�A�@׍P@�+@�-@Չ7@�Z@�
=@��@϶F@Ͳ-@�Z@˶F@��@�j@�1@�\)@��y@Ƈ+@�`B@�j@�Q�@�+@�@�bN@�o@�{@�|�@�9X@�A�@�\)@�hs@�{@��H@�9X@�J@���@¸R@�=q@�{@�bN@�=q@��@�V@��j@�;d@���@��@���@���@�J@��^@���@���@�@�bN@�@�Q�@���@��j@� �@���@�@�V@���@���@� �@��w@�@�
=@�33@��@���@���@�5?@�hs@�+@��@���@�j@��
@�K�@��@���@���@��#@���@��-@�?}@��@��-@��@�%@�(�@���@�;d@���@�ff@��@���@��h@�p�@�Q�@��F@�33@��@�S�@���@���@�G�@��j@��9@�Ĝ@�A�@��@�ƨ@��@��\@�{@���@�`B@�Z@�@�~�@�n�@�-@�ff@�E�@��#@���@�O�@�G�@�7L@�p�@��@��F@��@��@��@�l�@��y@��\@���@��h@�V@���@��@��@���@�hs@��/@��u@��D@��@���@��9@�Ĝ@��j@�Z@��@�ƨ@��P@�t�@�l�@�K�@��R@�ȴ@���@�@���@��@���@���@�^5@�-@��T@���@��7@�p�@��@�V@��`@��j@��u@�Q�@�I�@�1@��;@���@�"�@��y@��H@���@���@�V@��@��T@�hs@�7L@�%@���@��9@�r�@�b@��;@���@��w@�hs@z�!@o��@hĜ@` �@Y�^@Sƨ@M��@Fȴ@>ff@7�P@0��@,��@&5?@�P@�@?}@��@�@�;11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��`Aа!AХ�AЙ�AГuAЉ7AЇ+AЅAЃAЃAЁA�~�A�~�A�|�A�|�A�|�A�z�A�z�A�|�AЁAЋDAЏ\AЏ\A�jA��AͰ!A˛�A��A�I�Aȧ�A��A��#A�O�A�33A�x�A���A��A���A���A�A�?}A�E�A�I�A�t�A�ȴA�\)A�&�A�x�A��RA�%A��DA��A�=qA�O�A��A�x�A�C�A�JA��mA��jA��RA��FA���A��A��FA���A�^5A���A���A��A��RA�"�A���A���A�p�A��`A��+A�bA�
=A�VA���A�r�A�M�A�JA�M�A��;A�+A��A�-A���A��^A���A��A���A��#A�~�A��TA�`BA���A�ĜA�I�A���A��A�O�A��
A�r�A�bA���A��TA���A�`BA�r�A��A�A�A~��A}��A}t�A}&�A|�!A{��Ay33AwO�AvffAu33AshsAr��Ar�jArE�ArbAq�wAq7LAp��Ap�/Ao��An  AlbAi�PAh=qAf�AfM�Ad��Ab9XAaA`(�A^�9A\��AZ��AY+AWK�AU��AT�/AS�-AR��ARVAQ�#AQ��AQ\)APE�AO��AN9XAL9XAJȴAH1'AF��AEp�AE�AD��AC��AC�AC33AB�\AA�#AA�^AA�A@�A@ �A>�jA=�A;G�A9��A8-A6��A4Q�A2ȴA1�FA1"�A0 �A/A/�hA/C�A.n�A.$�A-�A,�A+ƨA*�A)x�A)C�A'A&jA%��A%\)A$�RA$$�A!A!K�A ��A �A�7A"�A��AK�A(�A��A%A5?A�A|�A�!A��A7LAZA-A�;A�A1'A�^A
=A�7A�`AQ�AO�AJA�7AVA^5Ap�A
�!A	G�AAr�AI�A�-A"�A�RAS�A1'A�A @��@���@�9X@��y@���@�1@�l�@�O�@���@�+@홚@�@�b@��@��@���@��@���@�l�@�+@�o@�ff@�bN@�ȴ@��@ݙ�@ܬ@ۍP@�v�@�p�@�Ĝ@�A�@׍P@�+@�-@Չ7@�Z@�
=@��@϶F@Ͳ-@�Z@˶F@��@�j@�1@�\)@��y@Ƈ+@�`B@�j@�Q�@�+@�@�bN@�o@�{@�|�@�9X@�A�@�\)@�hs@�{@��H@�9X@�J@���@¸R@�=q@�{@�bN@�=q@��@�V@��j@�;d@���@��@���@���@�J@��^@���@���@�@�bN@�@�Q�@���@��j@� �@���@�@�V@���@���@� �@��w@�@�
=@�33@��@���@���@�5?@�hs@�+@��@���@�j@��
@�K�@��@���@���@��#@���@��-@�?}@��@��-@��@�%@�(�@���@�;d@���@�ff@��@���@��h@�p�@�Q�@��F@�33@��@�S�@���@���@�G�@��j@��9@�Ĝ@�A�@��@�ƨ@��@��\@�{@���@�`B@�Z@�@�~�@�n�@�-@�ff@�E�@��#@���@�O�@�G�@�7L@�p�@��@��F@��@��@��@�l�@��y@��\@���@��h@�V@���@��@��@���@�hs@��/@��u@��D@��@���@��9@�Ĝ@��j@�Z@��@�ƨ@��P@�t�@�l�@�K�@��R@�ȴ@���@�@���@��@���@���@�^5@�-@��T@���@��7@�p�@��@�V@��`@��j@��u@�Q�@�I�@�1@��;@���@�"�@��y@��H@���@���@�V@��@��T@�hs@�7L@�%@���@��9@�r�@�b@��;@���@��w@�hs@z�!@o��@hĜ@` �@Y�^@Sƨ@M��@Fȴ@>ff@7�P@0��@,��@&5?@�P@�@?}@��@�@�;11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B�B��B��B�B�B�B�B�B�B�-B�3B�9B�RBBĜBǮBĜB�^B�B��B�fB�B9XBB�BK�BR�BT�BbNBiyBr�B|�B�7B��B��B�9B�RB�qB��B��B��B��B�{B�hB�hB��B��B�B�LB�XB�wB��B�;B�B��B��B��B��B��BȴBB�dB�FB�!B�B��B��B��B�JB�B}�Bz�Bw�Br�BffBR�B8RB�B+B�B�NB��B�?B��B�DBu�B\)BG�B/B#�B�B\BB
�B
ɺB
�wB
�XB
�FB
�B
��B
��B
�=B
�B
y�B
p�B
k�B
gmB
e`B
`BB
XB
H�B
?}B
:^B
8RB
49B
49B
49B
33B
2-B
1'B
/B
/B
.B
&�B
�B
VB	��B	�B	�yB	�B	�fB	�B	��B	��B	��B	�!B	��B	��B	�PB	�%B	}�B	u�B	o�B	l�B	l�B	l�B	m�B	iyB	ffB	_;B	YB	N�B	@�B	9XB	2-B	0!B	/B	-B	)�B	&�B	"�B	�B	#�B	"�B	�B	{B		7B��B�B�`B�BB�
BǮB��B��B�}B�dB�XB�RB�LB�3B�3B�?B�?B�!B�B��B��B��B��B��B��B��B��B��B��B�uB�hB�bB�\B�DB�1B�%B�1B�7B�1B�7B�1B�%B�B�B�B�B}�B|�Bz�Bw�Bt�Bq�Bo�Bl�BiyBgmBe`BcTB`BB^5B[#BXBVBT�BS�BR�BQ�BO�BN�BL�BJ�BH�BF�BE�BD�BC�BB�BC�BA�BA�B@�B?}BA�BA�B@�B>wB@�B;dB8RB7LB8RB8RB8RB8RB5?B9XB<jB=qB=qB<jB=qB>wB?}B>wB>wB>wB?}B?}B@�B?}B=qB;dB:^B<jB<jB<jB<jB<jB>wB=qB;dB:^B;dB?}BC�BC�BE�BF�BK�B[#B`BB`BB]/B^5Bl�Bv�B�B��B��B��B��B��B��B��B��B��B�B��B��B�B��B�B�3B�9B�LB�^B�XB�FB�-B�B��B��B�B��B��B��B��B��B��B��B��B�B�-B�3B�3B�3B�3B�3B�XB�XB�RB�RB�jB�}BÖBŢBȴB��B�B�5B�BB�HB�sB�B�B�B�B�B�B�B��B��B��B	  B	B	B	B	+B	PB	uB	�B	"�B	%�B	'�B	(�B	,B	/B	2-B	33B	49B	49B	6FB	9XB	;dB	H�B	P�B	Q�B	S�B	W
B	YB	\)B	\)B	[#B	[#B	[#B	`BB	_;B	bNB	dZB	ffB	gmB	jB	k�B	l�B	l�B	n�B	u�B	x�B	{�B	{�B	y�B	x�B	w�B	z�B	|�B	}�B	�B	�B	�B	�B	�%B	�1B	�1B	�DB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�-B	�-B	�?B	�LB	�RB	�XB	�jB	�qB	�wB	�}B	��B	��B	B	B	ÖB	ĜB	ƨB	ȴB	ɺB	��B	��B	�/B	�B	��B
uB
�B
%�B
,B
33B
<jB
A�B
H�B
M�B
R�B
VB
\)B
cTB
gmB
k�B
q�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�B�B�B�B�B�B�-B�3B�9B�jBǮB��BɺBȴB��B�9B��B�B�B<jBE�BN�BS�BYBdZBl�Bu�B�B�DB��B�B�RB�dBĜB��B��B��B��B��B�uB�uB��B��B�!B�RB�XB�wB��B�NB�#B��B�
B�B��B��B��BŢB�wB�dB�-B�B��B��B��B�bB�%B~�B{�Bx�Bu�Bk�BYB>wB#�BPB��B�mB�B�^B�B�oB}�BdZBQ�B49B&�B�BuB\B
��B
��B
��B
�jB
�jB
�LB
�B
��B
�VB
�1B
� B
s�B
m�B
hsB
gmB
dZB
aHB
N�B
C�B
>wB
=qB
5?B
5?B
5?B
49B
33B
2-B
0!B
/B
1'B
+B
"�B
{B	��B	��B	�B	�B	�B	�/B	�
B	��B	ǮB	�?B	��B	��B	�hB	�=B	�B	w�B	q�B	n�B	m�B	m�B	q�B	k�B	k�B	ffB	_;B	XB	F�B	=qB	49B	2-B	1'B	.B	+B	(�B	$�B	�B	$�B	$�B	�B	�B	PB	B�B�yB�ZB�/B��BÖBÖBB�jB�^B�XB�XB�9B�9B�RB�RB�?B�B�B�B��B��B��B��B��B��B��B��B��B�uB�oB�hB�\B�JB�1B�=B�JB�7B�DB�DB�7B�+B�%B�B�B�B� B|�Bz�Bz�Bt�Br�Bp�Bn�BiyBgmBffBdZBbNB`BB]/B\)BVBVBT�BR�BS�BQ�BN�BN�BL�BJ�BH�BF�BE�BE�BD�BD�BC�BA�BA�BC�BB�BB�B@�BE�B?}B8RB8RB9XB9XB:^B<jB8RB;dB=qB?}B?}B>wB?}B?}B@�B@�B?}B@�BA�BB�BC�BA�BB�B;dB=qB>wB@�B?}B=qB>wB>wB>wB>wB<jB<jBB�BF�BF�BG�BH�BI�BZB`BBbNB`BB]/Bk�Bt�B�B��B��B��B��B��B�B�B�B�B�B��B��B�B�B�B�9B�FB�LB�jB�jB�XB�-B�B�B��B�B�B��B��B��B��B��B��B��B�B�-B�9B�3B�9B�9B�LB�dB�dB�XB�XB�qB��BĜBƨBȴB��B�B�;B�BB�BB�sB�B�B�B�B�B�B��B��B��B��B	B	B	B	B	+B	JB	oB	�B	#�B	%�B	'�B	)�B	-B	0!B	49B	49B	5?B	5?B	7LB	;dB	9XB	G�B	P�B	Q�B	S�B	W
B	ZB	\)B	]/B	[#B	[#B	[#B	bNB	_;B	bNB	dZB	ffB	gmB	k�B	l�B	m�B	l�B	m�B	u�B	x�B	{�B	}�B	z�B	y�B	x�B	z�B	|�B	}�B	�B	�B	�B	�%B	�+B	�1B	�7B	�DB	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�B	�3B	�-B	�3B	�FB	�LB	�RB	�XB	�qB	�qB	�}B	��B	��B	��B	ÖB	ÖB	ÖB	ŢB	ǮB	ɺB	ɺB	��B	��B	�5B	�B	��B
uB
�B
&�B
,B
49B
<jB
A�B
H�B
M�B
R�B
VB
\)B
cTB
gmB
l�B
q�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446422012010314464220120103144642  AO  ARGQ                                                                        20111130135630  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135630  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144642  IP                  G�O�G�O�G�O�                