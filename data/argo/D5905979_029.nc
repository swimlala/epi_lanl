CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:08:59Z creation      
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
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170859  20220204114413  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؍���x1   @؍��J�@8^�Q��cۍO�;d1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�33@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBy33B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4�fD5  D5� D6  D6� D7  D7� D8  D8�fD9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DRy�DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D^��D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dq��Dr� Ds  Ds� Dt  Dt� Dt��Dy�{D�)HD�S3D���D�ҏD��D�S�D���D��HD� �D�S3D���D��RD�&D�eDڣ3D��\D��D�I�D�x D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @y��@�fg@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo��BxffB~��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1�gC3�gC5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D��Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4y�D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8y�D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRl�DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^��D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq��Drs3Dr�3Dss3Ds�3Dts3Dt� Dy��D�"�D�L�D��)D��)D�{D�MD��)D���D��D�L�D��RDǹ�D��D�^�Dڜ�D���D�4D�C4D�q�D�Æ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�v�A�~�A�|�A�~�AӃA�~�A�z�A�x�A�v�A�5?A���A���A���A���A�ȴA�AҾwAҺ^AҸRAҶFAҥ�A�t�A�I�AѼjA��A�v�A�  A�ƨA�XA��`A��A�;dA��TA�(�A��+A���A�K�A�G�A�hsA��\A��+A��#A���A�33A���A�~�A��uA�+A���A�9XA�1A���A���A���A� �A��+A�  A���A�=qA��HA��A�`BA�ZA���A�-A�%A�9XA�n�A�v�A�M�A��A��#A�\)A��FA�ZA��A��mA�r�A�9XA�jA��/A�A��A���A�bNA�A���A���A��\A��#A�1A��;A��wA�"�A���A�ĜA�|�A�7LA�|�A��A���A��A��`A���A��hA�A���A�dZA���A�PA{��Az�/Ax9XAt�At^5As`BAoC�AjĜAdM�Aa�-Aa33A`�jA_+A\��AYG�AT��AS�ARz�AQAPffAO�^AO7LAN�AMC�AK?}AI"�AH-AG?}AF5?AE�AD�uACC�AB��A@�/A?A?|�A?\)A>ȴA>1'A=�hA<ffA:�yA9x�A8ȴA85?A7�-A7`BA6Q�A5A6A6 �A5�PA4ZA1�A-�-A,1A*  A(ĜA'�TA&�uA%A#��A"��A"�RA"�uA"~�A"ffA!ƨA ĜA%A��AoAjAA�-A
=AE�A�A��AM�A��A��AAC�A�yA�
AXAA�A  AĜA�A&�A�A�FA?}A�AƨA�A
-A	�#A	G�A�#A�A�!A{AƨA�`Ap�A�uA|�@���@��7@�+@��7@��D@�ff@��j@�/@�@�V@�Q�@�1@�F@�K�@�+@睲@�ȴ@��@��@�!@���@���@旍@�M�@��@��@���@��@�@��@�(�@�|�@�@݁@�Ĝ@�o@�-@��`@�Q�@�A�@׮@�+@�$�@��/@� �@��H@�Ĝ@�z�@� �@��@�v�@�G�@�Z@��@�
=@��@ƸR@�x�@�Ĝ@��@��H@���@�Ĝ@���@���@���@�p�@��u@��@�-@���@�G�@��9@� �@�C�@�v�@�5?@���@�&�@��/@��@��9@��D@��@��m@��m@��;@���@��P@�K�@�+@�@���@��!@�~�@�V@�-@���@�Q�@��@�l�@�l�@���@���@�v�@��@��-@���@�/@���@�Ĝ@��D@�Q�@���@�\)@�C�@��@��@��F@�`B@��P@�Ĝ@�Z@�A�@��P@���@�-@�X@��@�r�@�A�@�  @�|�@�S�@�K�@�C�@�;d@�"�@�
=@��@��@�ȴ@��!@�ff@�J@���@�X@��@��@�/@�&�@���@��D@��u@�Z@�1@��P@��y@�ȴ@���@�~�@�~�@�ff@�-@���@�X@��j@�Z@�b@���@��@�
=@��H@���@�ff@�M�@�J@���@��@��@��T@���@���@�J@�J@��@�x�@���@��m@��!@���@��@��7@�V@��9@���@�z�@�bN@�9X@�1'@�1@���@���@��@��F@���@��@�l�@�@��y@��y@�M�@��@��T@���@���@�p�@�O�@�?}@��@��`@��`@��j@�(�@��m@�1'@���@��@�G�@�7L@��@���@��@��@��@�r�@�9X@���@�ƨ@��F@���@�dZ@�"�@���@���@���@�M�@�{@�@���@��7@�X@�/@��`@���@��@�I�@�(�@��;@��@���@�\)@�
=@��y@�^5@�5?@�5?@�=q@�+@v�@k��@fE�@`�@X�@P�p@I�@E?}@>Q@5�9@/�P@*�@%��@�A@�g@�@�@?�@�&@bN111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�v�A�~�A�|�A�~�AӃA�~�A�z�A�x�A�v�A�5?A���A���A���A���A�ȴA�AҾwAҺ^AҸRAҶFAҥ�A�t�A�I�AѼjA��A�v�A�  A�ƨA�XA��`A��A�;dA��TA�(�A��+A���A�K�A�G�A�hsA��\A��+A��#A���A�33A���A�~�A��uA�+A���A�9XA�1A���A���A���A� �A��+A�  A���A�=qA��HA��A�`BA�ZA���A�-A�%A�9XA�n�A�v�A�M�A��A��#A�\)A��FA�ZA��A��mA�r�A�9XA�jA��/A�A��A���A�bNA�A���A���A��\A��#A�1A��;A��wA�"�A���A�ĜA�|�A�7LA�|�A��A���A��A��`A���A��hA�A���A�dZA���A�PA{��Az�/Ax9XAt�At^5As`BAoC�AjĜAdM�Aa�-Aa33A`�jA_+A\��AYG�AT��AS�ARz�AQAPffAO�^AO7LAN�AMC�AK?}AI"�AH-AG?}AF5?AE�AD�uACC�AB��A@�/A?A?|�A?\)A>ȴA>1'A=�hA<ffA:�yA9x�A8ȴA85?A7�-A7`BA6Q�A5A6A6 �A5�PA4ZA1�A-�-A,1A*  A(ĜA'�TA&�uA%A#��A"��A"�RA"�uA"~�A"ffA!ƨA ĜA%A��AoAjAA�-A
=AE�A�A��AM�A��A��AAC�A�yA�
AXAA�A  AĜA�A&�A�A�FA?}A�AƨA�A
-A	�#A	G�A�#A�A�!A{AƨA�`Ap�A�uA|�@���@��7@�+@��7@��D@�ff@��j@�/@�@�V@�Q�@�1@�F@�K�@�+@睲@�ȴ@��@��@�!@���@���@旍@�M�@��@��@���@��@�@��@�(�@�|�@�@݁@�Ĝ@�o@�-@��`@�Q�@�A�@׮@�+@�$�@��/@� �@��H@�Ĝ@�z�@� �@��@�v�@�G�@�Z@��@�
=@��@ƸR@�x�@�Ĝ@��@��H@���@�Ĝ@���@���@���@�p�@��u@��@�-@���@�G�@��9@� �@�C�@�v�@�5?@���@�&�@��/@��@��9@��D@��@��m@��m@��;@���@��P@�K�@�+@�@���@��!@�~�@�V@�-@���@�Q�@��@�l�@�l�@���@���@�v�@��@��-@���@�/@���@�Ĝ@��D@�Q�@���@�\)@�C�@��@��@��F@�`B@��P@�Ĝ@�Z@�A�@��P@���@�-@�X@��@�r�@�A�@�  @�|�@�S�@�K�@�C�@�;d@�"�@�
=@��@��@�ȴ@��!@�ff@�J@���@�X@��@��@�/@�&�@���@��D@��u@�Z@�1@��P@��y@�ȴ@���@�~�@�~�@�ff@�-@���@�X@��j@�Z@�b@���@��@�
=@��H@���@�ff@�M�@�J@���@��@��@��T@���@���@�J@�J@��@�x�@���@��m@��!@���@��@��7@�V@��9@���@�z�@�bN@�9X@�1'@�1@���@���@��@��F@���@��@�l�@�@��y@��y@�M�@��@��T@���@���@�p�@�O�@�?}@��@��`@��`@��j@�(�@��m@�1'@���@��@�G�@�7L@��@���@��@��@��@�r�@�9X@���@�ƨ@��F@���@�dZ@�"�@���@���@���@�M�@�{@�@���@��7@�X@�/@��`@���@��@�I�@�(�@��;@��@���@�\)@�
=@��y@�^5@�5?@�5?G�O�@�+@v�@k��@fE�@`�@X�@P�p@I�@E?}@>Q@5�9@/�P@*�@%��@�A@�g@�@�@?�@�&@bN111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B�/B�B�B�B�B�B�B�B��B��B��BJB$�B9XBC�BD�BE�BP�BXB_;BgmBq�B� B~�B�%B�7B�DB�7B�DB�+B�%B}�By�Bw�Bq�Bq�By�B{�B|�B~�B� B�B�B�B�B�B�B�B�B�%B�1B�\B�PB�JB�1B�B�B~�Bz�Bo�Bm�Bk�BhsB^5BS�BO�BL�BI�BC�B6FB �BJB�B�;B�B��B��BɺB�}B�B��B�1Bs�BT�B5?B-B�BDBB
��B
��B
�'B
��B
��B
�=B
u�B
jB
dZB
_;B
R�B
A�B
�B
	7B	�B	��B	��B	�`B	�^B	�PB	_;B	L�B	G�B	E�B	>wB	5?B	�B�B�TB�)B�5B�HB�;B�)B�B��B��B�qB�FB�'B�B��B��B��B��B�uB�\B�VB�VB�\B�\B�VB�\B�JB�DB�bB��B��B��B�uB�uB��B��B��B��B�7Bw�Bp�BhsBbNBbNBbNB`BB_;B^5B^5B^5B^5B^5B^5BZBXBT�BS�BT�BVBT�BR�BR�BO�BN�BN�BL�BJ�BH�BF�BE�BC�BA�B@�B@�B=qB=qB;dB:^B8RB7LB7LB6FB6FB5?B49B2-B2-B/B-B-B+B+B+B(�B(�B'�B&�B"�B$�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B"�B#�B%�B'�B,B/B0!B0!B49B5?B6FB8RB9XB>wB?}BA�B@�B@�BE�BK�BK�BM�BS�B[#B^5B^5B_;B`BBdZBe`Bk�Bn�Bm�Bl�BiyBhsBiyBk�Bl�Bm�Bn�Br�Bs�Bv�Bv�Bz�B}�B�B�%B�+B�DB�PB�bB�oB�uB��B��B��B��B��B��B�B�-B�3B�3B�9B�RB�^B�^B�qB�}B��BÖBŢBƨBƨB��B��B�B�B�5B�5B�HB�mB�sB�B�B�B��B��B��B��B��B��B��B��B��B�B�B�TB�HB�NB�NB�HB�HB�TB�`B�B�B��B��B��B	  B	B	B	B	B	B	B	B	%B	+B		7B	JB	bB	uB	{B	�B	�B	 �B	$�B	'�B	(�B	(�B	+B	0!B	0!B	0!B	33B	49B	7LB	:^B	?}B	F�B	L�B	O�B	R�B	W
B	]/B	]/B	_;B	dZB	gmB	iyB	l�B	m�B	o�B	p�B	r�B	u�B	y�B	�B	�B	�B	�B	�B	�B	�B	}�B	|�B	{�B	}�B	� B	�B	�B	�7B	�PB	�PB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�LB	�LB	�RB	�dB	��B	ĜB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�/B	�/B	�/B	�;B	�HB	�NB	�TB	�ZB	�`B	�`B	�fB	�fB	�mB	�yB	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�nB
-B
�B
,B
B
#:B
+kB
1�B
88B
?cB
HB
N<B
SB
X+B
^�B
b�B
gRB
k�B
q'B
u�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�&B�&B�&B� B�&B�&B�&B�&B�,BԁB��B��B��B��B�B�B�B�B�B�JB�B+B0�B:�B;�B<�BH1BO\BV�B^�Bh�BwJBvEB}pB��B��B��B��B~wB}qBuABq)BoBh�Bh�Bq)Bs5Bt<BvHBwNBxTBxTBxTBy[BxUBxUBzaB{hB}tB�B��B��B��B�B{iBzbBvJBr2Bf�Bd�Bb�B_�BU�BKLBG3BD"BAB:�B-�BB�B��B֘B�nB�\B�8B�B��B�oB�-B�BkBLfB,�B$yB*B�B
��B
�?B
�XB
��B
�YB
�(B
��B
m=B
a�B
[�B
V�B
JnB
9B
B
 �B	�(B	�ZB	�kB	��B	��B	��B	V�B	D^B	?@B	=4B	6	B	,�B	5B�CB��B��B��B��B��B��BѸBʎB�]B�B��B��B��B��B�kB�FB�;B�B��B��B��B��B��B��B��B��B��B�B�/B�#B�#B�B�B�<B�BB�BB�*B��BouBhJB`BY�BY�BY�BW�BV�BU�BU�BU�BU�BU�BU�BQ�BO�BL�BK�BL�BM�BL�BJ�BJ�BG�BF�BF�BDxBBmB@`B>TB=NB;CB96B80B80B5B5B3B2B0 B.�B.�B-�B-�B,�B+�B)�B)�B&�B$�B$�B"�B"�B"�B �B �B�B�B�B�B}BwBpBeB_BGBYBYBYBYBZBZBZBZBfBlBlByByB�B�B�B�B#�B&�B'�B'�B+�B,�B-�B0B1B6*B70B9;B86B86B=TBCyBCyBE�BK�BR�BU�BU�BV�BW�B\B]Bc6BfIBeBBd<Ba*B`%Ba*Bc6Bd<BeBBfIBjaBkgBnzBnzBr�Bu�B{�B}�B~�B��B� B�B�B�%B�7B�PB�mB�mB�zB��B��B��B��B��B��B� B�B�B�B�*B�0B�CB�OB�UB�UBƆBʞBͰB��B��B��B��B�B�B�*B�CB�[B�fB�sB�sB�B��B��B��B��B�B�OB�1B�B��B��B��B��B��B�B�B�7B�VB�{B��B��B��B��B��B��B��B��B��B��B��B��B	 �B	�B	B	B	%B	+B	7B	nB	�B	�B	 �B	 �B	"�B	'�B	'�B	'�B	*�B	+�B	.�B	2B	7$B	>OB	DtB	G�B	J�B	N�B	T�B	T�B	V�B	[�B	_B	aB	d0B	e6B	gCB	hIB	jTB	mgB	qB	x�B	y�B	z�B	z�B	z�B	y�B	x�B	u�B	t�B	s�B	u�B	w�B	x�B	|�B	��B	��B	��B	��B	��B	�B	�$B	�0B	�<B	�<B	�<B	�=B	�HB	�TB	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�<B	�BB	�HB	�NB	�TB	�lB	ȄB	ɋB	ʑB	˗B	̝B	ͣB	ΩB	ϯB	жB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�6G�O�B	�B	��B
MB
�B
�B
�B
#B
)�B
/�B
6�B
?�B
E�B
J�B
O�B
V�B
ZkB
^�B
c�B
h�B
m'B
q@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144132022020411441320220204114413  AO  ARCAADJP                                                                    20200619170859    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170859  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170859  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114413  IP                  G�O�G�O�G�O�                