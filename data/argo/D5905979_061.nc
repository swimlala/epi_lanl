CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:08Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
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
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �\   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170908  20220204114416  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               =A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ت]�r6�1   @ت^$��@6�Z�1�c����l�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    =A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C�fC  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!�fD"fD"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,� D-  D-� D.  D.� D/  D/�fD0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZy�D[  D[� D\  D\� D]  D]� D]��D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dc��Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D��D�W�D���D��RD�!�D�aHD��=D��D�!HD�K�D��\D��RD�"�D�T)Dڕ�D��D�RD�R�D�R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�34@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BF��BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C�gC��C	��C��C��C��C��C��C��C�3C��C��C��C��C!�gC#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq�gCs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٙC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٙC��fC��fC��fC��fC��fC��fC��fC��fC�ٙC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٙD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!y�D!��D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+l�D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/y�D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZl�DZ�3D[s3D[�3D\s3D\�3D]s3D]��D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc��Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhl�Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt� Dy�D�qD�QHD��4D���D��D�Z�D���D��>D��D�ED���D���D�)D�M�Dڏ]D�ֹD�	�D�L{D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�E�A�5?A�=qA�?}A�A�A�C�A�E�A�M�A�M�A�M�A�M�A�G�A�1'A��A���A��PA�`BA�A�A�-A�"�A��A�  A��
A��!A���A���A��hA��+A��A��DA�~�A�ffA�VA�1'A�+A� �A��A�1A�A�  A���A��TA��;A��
A���A���A��A���A�ƨA��A�-A�I�A�r�A�{A�ĜA��HA�ZA��A�/A��DA�=qA�M�A��9A�A�A���A�1'A���A�A��yA�x�A��mA�9XA��;A�M�A��!A��
A��wA��-A��PA���A��A�?}A��#A���A��#A�JA�XA���A�?}A���A���A�S�A��PA��A��/A��+A��wA�jA�JA}A{t�Ay?}Ay+Ax�Av�Ar(�Ap��An^5Akl�Ah�Ae�A`�A_l�A^z�A]��A]XA[��AY�#AX$�AW&�AU�#AS�AR��AP�9AO��AO?}AM�mAL1'AK�AK�-AK�7AJ��AG�AFQ�AE�7AD$�ABJAA%A?�FA>I�A=��A<��A;%A8��A7��A7��A7A5��A5;dA4ȴA3��A2�RA1&�A//A/�A.�/A-�A+�A+?}A*��A)��A)?}A(��A'�#A'/A&M�A%�^A$��A$��A$1'A#��A"ȴA!��A!S�A��A9XA�mAĜA�7A�`A�AVA�9A/AI�AƨA��A�`A�;A�jAXA��AM�A\)A��A
�A
��A
ZA
=qA	�
AVA�AĜA1'AK�A�-AS�@��H@�7L@��P@�K�@� �@�^5@�h@�@��m@�+@@�@�X@�9@�@�{@�%@�A�@�+@�M�@�7L@�b@��m@��@�V@�p�@���@�~�@���@�1'@���@ۍP@���@�5?@ى7@ش9@���@ՙ�@�7L@�Ĝ@ӶF@�+@���@�@мj@�ƨ@�~�@�X@�`B@�`B@�Q�@�(�@˶F@ˍP@�;d@�ȴ@���@�&�@��
@��@ě�@�1@î@�33@�ff@���@���@�bN@�b@��@�dZ@�
=@�ff@��@�G�@��@��j@���@�"�@���@�x�@���@���@�+@��y@��@�&�@���@���@�(�@�t�@�v�@�5?@�@��h@�V@��j@�9X@��@�  @���@���@�n�@�M�@��#@��-@��@�G�@�%@��`@��/@��`@��9@���@�j@��
@���@�K�@�~�@�@�p�@��j@�r�@�Z@��;@�@�@��@�p�@�`B@�G�@��@��D@���@���@�dZ@�33@��@��R@�$�@��-@��h@�`B@���@�bN@�t�@���@��@��R@��\@�-@�J@��@��@��T@���@��^@��@��@�r�@�I�@�1'@� �@��@�b@��@�t�@�"�@��@�@�n�@�n�@�ff@�ff@�V@��@�X@��@��D@�1'@�(�@�1'@��m@���@�t�@�dZ@�33@�+@�@��!@��\@�v�@�n�@�V@�=q@��@�@���@���@��@�hs@�%@���@��9@�z�@� �@��@�C�@��@���@��+@�n�@�ff@�E�@�@��T@���@��@���@�9X@�I�@�r�@��@�j@�bN@�Q�@�9X@�1'@� �@�1@��@�t�@�
=@��y@�ȴ@�^5@�{@���@�/@���@��D@�(�@�  @��@��@�V@�hs@�?}@�V@��`@�Ĝ@���@�z�@�Z@�9X@�1'@��@��@��@���@�|�@�33@�+@��@�ȴ@���@��R@��R@��!@��+@�E�@��/@�9X@�b@��m@��m@��m@��m@��m@�r�@���@�@|�@sv`@h��@`?�@Y@O�@J��@B�@;F�@4e�@.��@*J@$$@��@'�@�@  @�`@
=q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   A�A�A�E�A�5?A�=qA�?}A�A�A�C�A�E�A�M�A�M�A�M�A�M�A�G�A�1'A��A���A��PA�`BA�A�A�-A�"�A��A�  A��
A��!A���A���A��hA��+A��A��DA�~�A�ffA�VA�1'A�+A� �A��A�1A�A�  A���A��TA��;A��
A���A���A��A���A�ƨA��A�-A�I�A�r�A�{A�ĜA��HA�ZA��A�/A��DA�=qA�M�A��9A�A�A���A�1'A���A�A��yA�x�A��mA�9XA��;A�M�A��!A��
A��wA��-A��PA���A��A�?}A��#A���A��#A�JA�XA���A�?}A���A���A�S�A��PA��A��/A��+A��wA�jA�JA}A{t�Ay?}Ay+Ax�Av�Ar(�Ap��An^5Akl�Ah�Ae�A`�A_l�A^z�A]��A]XA[��AY�#AX$�AW&�AU�#AS�AR��AP�9AO��AO?}AM�mAL1'AK�AK�-AK�7AJ��AG�AFQ�AE�7AD$�ABJAA%A?�FA>I�A=��A<��A;%A8��A7��A7��A7A5��A5;dA4ȴA3��A2�RA1&�A//A/�A.�/A-�A+�A+?}A*��A)��A)?}A(��A'�#A'/A&M�A%�^A$��A$��A$1'A#��A"ȴA!��A!S�A��A9XA�mAĜA�7A�`A�AVA�9A/AI�AƨA��A�`A�;A�jAXA��AM�A\)A��A
�A
��A
ZA
=qA	�
AVA�AĜA1'AK�A�-AS�@��H@�7L@��P@�K�@� �@�^5@�h@�@��m@�+@@�@�X@�9@�@�{@�%@�A�@�+@�M�@�7L@�b@��m@��@�V@�p�@���@�~�@���@�1'@���@ۍP@���@�5?@ى7@ش9@���@ՙ�@�7L@�Ĝ@ӶF@�+@���@�@мj@�ƨ@�~�@�X@�`B@�`B@�Q�@�(�@˶F@ˍP@�;d@�ȴ@���@�&�@��
@��@ě�@�1@î@�33@�ff@���@���@�bN@�b@��@�dZ@�
=@�ff@��@�G�@��@��j@���@�"�@���@�x�@���@���@�+@��y@��@�&�@���@���@�(�@�t�@�v�@�5?@�@��h@�V@��j@�9X@��@�  @���@���@�n�@�M�@��#@��-@��@�G�@�%@��`@��/@��`@��9@���@�j@��
@���@�K�@�~�@�@�p�@��j@�r�@�Z@��;@�@�@��@�p�@�`B@�G�@��@��D@���@���@�dZ@�33@��@��R@�$�@��-@��h@�`B@���@�bN@�t�@���@��@��R@��\@�-@�J@��@��@��T@���@��^@��@��@�r�@�I�@�1'@� �@��@�b@��@�t�@�"�@��@�@�n�@�n�@�ff@�ff@�V@��@�X@��@��D@�1'@�(�@�1'@��m@���@�t�@�dZ@�33@�+@�@��!@��\@�v�@�n�@�V@�=q@��@�@���@���@��@�hs@�%@���@��9@�z�@� �@��@�C�@��@���@��+@�n�@�ff@�E�@�@��T@���@��@���@�9X@�I�@�r�@��@�j@�bN@�Q�@�9X@�1'@� �@�1@��@�t�@�
=@��y@�ȴ@�^5@�{@���@�/@���@��D@�(�@�  @��@��@�V@�hs@�?}@�V@��`@�Ĝ@���@�z�@�Z@�9X@�1'@��@��@��@���@�|�@�33@�+@��@�ȴ@���@��R@��R@��!@��+@�E�@��/@�9X@�b@��m@��m@��m@��m@��m@�r�G�O�@�@|�@sv`@h��@`?�@Y@O�@J��@B�@;F�@4e�@.��@*J@$$@��@'�@�@  @�`@
=q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB'�B&�B&�B&�B%�B%�B%�B%�B%�B%�B&�B%�B&�B+B,B.B,B.B1'B2-B2-B2-B33B2-B/B/B1'B1'B1'B1'B6FB9XB:^B?}B@�B@�B@�B@�B@�B@�BA�BA�B@�B?}B?}B?}B?}B@�BI�BO�BhsB{�Bv�Bv�Bu�Bm�BgmB_;BVBH�B@�B<jB5?B'�B�BhB
=B+BB�B�B�ZB�;BB�B��B��B��B�{B�oB�1B}�Bq�BbNBO�BA�B7LB.B&�B�BB
��B
�B
�B
�RB
��B
��B
�{B
�bB
|�B
cTB
P�B
;dB
=qB
=qB
-B
�B
+B	��B	�NB	��B	��B	��B	��B	�{B	�JB	�1B	�B	t�B	o�B	iyB	hsB	aHB	\)B	S�B	L�B	I�B	E�B	:^B	8RB	7LB	49B	33B	$�B	�B	�B	hB	+B	  B��B�B�B�B�ZB�B��B��B��B��BĜBÖB�jB�RB�-B��B��B��B��B�uB�JB�DB�+B�B�B{�Bz�Bx�Bu�Br�Bp�Bo�Bn�BjBffBe`B^5BYBN�BF�BC�B@�B?}B<jB;dB<jB9XB8RB7LB7LB5?B49B49B2-B1'B2-B1'B0!B0!B/B.B.B.B-B,B+B'�B+B'�B&�B$�B"�B'�B$�B$�B$�B#�B$�B#�B#�B$�B#�B#�B#�B$�B#�B$�B$�B$�B$�B$�B$�B%�B%�B%�B'�B(�B)�B+B+B+B,B,B-B-B1'B2-B2-B33B5?B6FB6FB8RB:^B<jBA�BE�BD�BD�BG�BG�BH�BH�BI�BJ�BM�BQ�BT�BZB^5B`BBbNBe`BgmBiyBm�Bp�Bq�Bq�Bs�Bv�B{�B� B�B�B�B�+B�7B�JB�oB�uB��B��B��B��B��B��B��B��B�B�3B�9B�?B�RB�dB�wBBBBŢB��B��B��B��B�
B�B�)B�NB�ZB�`B�mB�B�B�B��B��B��B��B��B��B	B	B	B	+B	DB	hB	uB	uB	{B	{B	�B	�B	�B	�B	�B	�B	 �B	"�B	(�B	,B	-B	/B	1'B	6FB	=qB	A�B	B�B	C�B	D�B	H�B	I�B	I�B	J�B	J�B	K�B	L�B	N�B	R�B	ZB	\)B	]/B	^5B	^5B	_;B	aHB	gmB	k�B	n�B	w�B	}�B	�B	�B	�B	�%B	�1B	�=B	�=B	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�3B	�9B	�?B	�FB	�RB	�^B	�^B	�dB	�jB	�jB	�jB	�jB	�jB	�jB	�jB	�qB	�wB	��B	B	ÖB	ĜB	ƨB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�)B	�/B	�/B	�/B	�/B	�/B	�/B	�/B	�/B	�)B	�BB	�BB	�BB	�HB	�HB	�NB	�`B	�mB	�9B	��B
	�B
�B
�B
(�B
,�B
2�B
<B
CB
JrB
N�B
S�B
Y�B
\�B
b�B
k�B
q'B
t�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B#B$	B&B$	B&B)(B*.B*.B*.B+4B*.B'B'B)(B)(B)(B)(B.GB1YB2^B7}B8�B8�B8�B8�B8�B8�B9�B9�B8�B7}B7}B7}B7}B8�BA�BG�B`rBs�Bn�Bn�Bm�Be�B_nBW<BNB@�B8�B4nB-DB�B�B	pBEB�3B�B�B�B�eB�GB��B�*B��B��B��B��B��B�CBvBi�BZcBG�B9�B/dB&-BB�B
�5B
��B
�B
�/B
�sB
�B
��B
��B
��B
uB
[|B
IB
3�B
5�B
5�B
%:B
�B	�ZB	�B	ڀB	�B	��B	�B	��B	��B	��B	�jB	zEB	l�B	g�B	a�B	`�B	Y�B	TeB	L5B	EB	A�B	=�B	2�B	0�B	/�B	,yB	+sB	B	�B	�B		�B�nB�DB�8B��B��B��BܠB�^B�4B�-B�!B�
B��B��B��B��B�xB�;B�AB�5B�B��B��B��ByB{`ByUBt6Bs0Bq$BnBk Bh�Bg�Bf�Bb�B^�B]�BV�BQiBG,B>�B;�B8�B7�B4�B3�B4�B1�B0�B/�B/�B-�B,�B,�B*�B)~B*�B)~B(xB(xB'rB&kB&lB&lB%fB$`B#ZB IB#[B IBBB7B+B JB7B7B8B2B8B2B2B8B2B2B2B8B2B8B8B8B8B8B8B>B?B?B LB!RB"XB#^B#^B#^B$dB$dB%jB%jB)�B*�B*�B+�B-�B.�B.�B0�B2�B4�B9�B=�B<�B<�B@	B@	BABABBBCBF.BJGBMYBRwBV�BX�BZ�B]�B_�Ba�Be�Bh�BjBjBlBo"Bt@BxYBzeBzeBzeB�B��B��B��B��B��B��B�B�(B�:B�@B�GB�SB�eB��B��B��B��B��B��B��B��B��B��B�B�4B�AB�SB�_B�eB�~BڢBܮBݴB��B��B��B��B�B�B�B�(B�AB�MB�eB�lB�lB�}B	�B		�B	�B	�B	�B	�B	�B	�B	�B	B		B	B	B	"B	!GB	$YB	%_B	'lB	)wB	.�B	5�B	9�B	:�B	;�B	<�B	AB	B	B	B	B	CB	CB	DB	EB	G(B	KAB	RkB	TwB	U}B	V�B	V�B	W�B	Y�B	_�B	c�B	f�B	pB	v@B	zXB	}kB	}kB	~qB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�'B	�'B	�-B	�3B	�LB	�XB	�^B	�^B	�eB	�eB	�wB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�!B	�!B	�4B	�:B	�@B	�FB	�FB	�@B	�@B	�@B	�5B	�"B	�B	�5B	�;B	�;B	�;B	�;B	�AB	�AB	�GB	�GB	�LB	�LB	�LB	�RB	�XB	�_B	�eB	�eB	�qB	�wB	�wB	�wB	�wB	�wB	�wB	�wB	�wB	�qB	؊B	؊B	؊B	ِB	ِB	ږB	ݨG�O�B	�B	�B
�B
B
-B
 �B
$�B
+*B
4GB
;WB
B�B
F�B
K�B
Q�B
U?B
Z�B
c�B
ijB
l�B
n:11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144162022020411441620220204114416  AO  ARCAADJP                                                                    20200619170908    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170908  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170908  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114416  IP                  G�O�G�O�G�O�                