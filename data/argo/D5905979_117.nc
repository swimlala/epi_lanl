CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:21Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170921  20220204114422  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               uA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��\�&\A1   @��]5�<@5�bM���c.$�/�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    uA   B   B   @�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C{�fC~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD�fDE  DE� DF  DF� DF��DG� DH  DH� DI  DI� DJ  DJy�DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D�RD�\)D��HD��RD�qD�b�D��D��D�$)D�S�D���D��)D�!HD�VfDڄ�D��)D�HD�[�D�D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�fg@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO��BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C�3C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{�3C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٙC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6l�D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDy�DD�3DEs3DE�3DFs3DF��DGs3DG�3DHs3DH�3DIs3DI�3DJl�DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�fDy�D��D�U�D���D���D�D�\)D���D��)D��D�MD��{D���D��D�P D�~gD���D��D�UD�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A���A���A��A���A���A���A���A�  A�A�1A�1A�JA�JA�
=A�
=A�VA�JA�JA�VA�JA�VA�VA�VA�bA�bA�bA�bA�oA�oA�{A�{A��A��A�{A��A��A�{A�{A��A�VA�  A���AѰ!A���Aɕ�A�9XA��yAĴ9A�r�Aç�A�VA�Q�A��A��TA��HA���A��A���A�^5A��A�l�A�VA��A���A��A�9XA��A��\A��7A���A��A��A�VA�$�A�v�A�\)A��/A��!A�jA�&�A�t�A�(�A� �A��/A���A��A�|�A�
=A�p�A��A��;A���A��A�&�A�ZA��A��
A�bNA��A��A��A�z�A��A��!A�ƨA���A��+A��FA�ĜA��-A��A��yA�t�A���A���A�=qA���A|�A}�TA|VA{��Azr�AwƨAt-ArJApM�Am
=AiC�AdȴAa�A_l�A[&�AY�AW�AVr�ARZAP��AOVAL��AL=qAKdZAI7LAF=qAD��AC\)AA�-A@A�A?A>�9A>=qA<��A;�mA9p�A81A6�jA4�A3A2 �A0��A0A/&�A-7LA+�#A*=qA)"�A(n�A(I�A'�A&�yA%��A$ffA#�FA#t�A#oA"1A �A��AoA�A=qAhsA��A�^A��An�An�A(�AA��A33Az�A5?AI�A��A�A(�AO�Ap�Ap�A��AȴAXA��AM�A��A?}A
�A	�A	�AI�A"�AhsA=qAS�A�TA
=A ~�@���@�x�@��@�=q@�G�@�bN@�t�@�9X@�C�@�@@�u@�o@�+@�@�`B@�`B@�D@�9X@�33@��@��@��@��
@�~�@��@�h@�9@���@�ƨ@߅@�`B@�dZ@��@�Ĝ@�;d@��@Ԭ@ӕ�@�+@���@�r�@Χ�@́@̛�@�n�@�Z@�o@Ý�@�j@�%@�Z@�bN@�l�@�$�@��T@�?}@��@�A�@�S�@��#@��@�Q�@�33@��7@��j@�|�@��@���@�^5@�J@���@���@�r�@���@�;d@���@��+@��@���@���@��@���@�+@��@��;@�Z@���@�l�@�S�@�\)@�@���@�~�@���@��@��+@��^@��`@�|�@��y@���@� �@�=q@���@���@�  @�
=@��R@��h@� �@�"�@�ȴ@��\@�v�@�M�@�v�@�"�@�
=@���@�p�@�X@�`B@��/@�V@���@�  @�=q@��@�7L@�?}@��@�p�@��T@��@�$�@��#@��T@�@�/@���@�|�@�dZ@�33@�ȴ@�~�@��\@��R@��@�ff@���@��@�hs@�&�@��9@��@�r�@��D@��u@���@��j@��j@�j@��@��m@��P@�l�@�;d@�
=@���@���@���@�ff@�$�@�@���@�p�@���@��@�bN@�A�@���@��
@�ƨ@��F@�dZ@�C�@�33@�
=@���@��+@�v�@�n�@�n�@�V@�E�@��@�x�@�O�@�G�@�G�@�?}@�7L@�7L@�&�@��`@�r�@�I�@�9X@��@��;@��F@��F@���@�S�@�o@���@���@�v�@�V@�E�@�$�@���@��@�`B@�X@�G�@��@��/@��@��D@�j@�9X@�b@���@��w@��P@�33@�@�ȴ@��\@�M�@�J@��^@�X@�&�@��@���@��D@�r�@�j@�bN@�Q�@��@��@���@�dZ@�33@��y@���@��+@�V@�@��7@�?}@�/@�&�@�%@���@��u@�r�@|z�@vL0@l�j@g�@`�p@Z4@So@L֡@G��@B@9k�@1T�@.GE@(֡@#�@�d@�~@��@��@=@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A���A���A��A���A���A���A���A�  A�A�1A�1A�JA�JA�
=A�
=A�VA�JA�JA�VA�JA�VA�VA�VA�bA�bA�bA�bA�oA�oA�{A�{A��A��A�{A��A��A�{A�{A��A�VA�  A���AѰ!A���Aɕ�A�9XA��yAĴ9A�r�Aç�A�VA�Q�A��A��TA��HA���A��A���A�^5A��A�l�A�VA��A���A��A�9XA��A��\A��7A���A��A��A�VA�$�A�v�A�\)A��/A��!A�jA�&�A�t�A�(�A� �A��/A���A��A�|�A�
=A�p�A��A��;A���A��A�&�A�ZA��A��
A�bNA��A��A��A�z�A��A��!A�ƨA���A��+A��FA�ĜA��-A��A��yA�t�A���A���A�=qA���A|�A}�TA|VA{��Azr�AwƨAt-ArJApM�Am
=AiC�AdȴAa�A_l�A[&�AY�AW�AVr�ARZAP��AOVAL��AL=qAKdZAI7LAF=qAD��AC\)AA�-A@A�A?A>�9A>=qA<��A;�mA9p�A81A6�jA4�A3A2 �A0��A0A/&�A-7LA+�#A*=qA)"�A(n�A(I�A'�A&�yA%��A$ffA#�FA#t�A#oA"1A �A��AoA�A=qAhsA��A�^A��An�An�A(�AA��A33Az�A5?AI�A��A�A(�AO�Ap�Ap�A��AȴAXA��AM�A��A?}A
�A	�A	�AI�A"�AhsA=qAS�A�TA
=A ~�@���@�x�@��@�=q@�G�@�bN@�t�@�9X@�C�@�@@�u@�o@�+@�@�`B@�`B@�D@�9X@�33@��@��@��@��
@�~�@��@�h@�9@���@�ƨ@߅@�`B@�dZ@��@�Ĝ@�;d@��@Ԭ@ӕ�@�+@���@�r�@Χ�@́@̛�@�n�@�Z@�o@Ý�@�j@�%@�Z@�bN@�l�@�$�@��T@�?}@��@�A�@�S�@��#@��@�Q�@�33@��7@��j@�|�@��@���@�^5@�J@���@���@�r�@���@�;d@���@��+@��@���@���@��@���@�+@��@��;@�Z@���@�l�@�S�@�\)@�@���@�~�@���@��@��+@��^@��`@�|�@��y@���@� �@�=q@���@���@�  @�
=@��R@��h@� �@�"�@�ȴ@��\@�v�@�M�@�v�@�"�@�
=@���@�p�@�X@�`B@��/@�V@���@�  @�=q@��@�7L@�?}@��@�p�@��T@��@�$�@��#@��T@�@�/@���@�|�@�dZ@�33@�ȴ@�~�@��\@��R@��@�ff@���@��@�hs@�&�@��9@��@�r�@��D@��u@���@��j@��j@�j@��@��m@��P@�l�@�;d@�
=@���@���@���@�ff@�$�@�@���@�p�@���@��@�bN@�A�@���@��
@�ƨ@��F@�dZ@�C�@�33@�
=@���@��+@�v�@�n�@�n�@�V@�E�@��@�x�@�O�@�G�@�G�@�?}@�7L@�7L@�&�@��`@�r�@�I�@�9X@��@��;@��F@��F@���@�S�@�o@���@���@�v�@�V@�E�@�$�@���@��@�`B@�X@�G�@��@��/@��@��D@�j@�9X@�b@���@��w@��P@�33@�@�ȴ@��\@�M�@�J@��^@�X@�&�@��@���@��D@�r�@�j@�bN@�Q�@��@��@���@�dZ@�33@��y@���@��+@�V@�@��7@�?}@�/@�&�@�%@���@��uG�O�@|z�@vL0@l�j@g�@`�p@Z4@So@L֡@G��@B@9k�@1T�@.GE@(֡@#�@�d@�~@��@��@=@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
ffB
e`B
ffB
ffB
ffB
ffB
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
ffB
gmB
gmB
ffB
e`B
gmB
gmB
hsB
hsB
gmB
gmB
hsB
e`B
e`B
e`B
e`B
e`B
ffB
e`B
gmB
gmB
gmB
hsB
gmB
iyB
jB
l�B
m�B
t�B
�uBcTB`BBs�Bx�B�=B��B�B�-B�B�B�LBɺB�B�BB%B�B�B�B�B�B�B�B"�B!�B�B�B�B�B�B�B%�B�B�B�B�B�BuBJBJB  B�yB��B��B�ZB�`B�NB�BB�)B��B�wB�B��B��B��B�DBt�BdZBVBD�B8RB$�B�B\B
�B
�B
ŢB
�9B
��B
z�B
bNB
\)B
N�B
A�B
6FB
'�B
�B
{B
B	�B	�B	ǮB	�!B	��B	z�B	iyB	[#B	H�B	=qB	2-B	'�B	bB		7B	%B��B	B	PB	B��B�B�B�`B�5B�
B�B��B��BǮB�RB�!B�B��B��B��B��B�oB�\B�DB�%B�B}�Bz�By�Bw�Bu�Bq�Bn�Bk�BjBiyBgmBdZBbNB_;B^5B^5B\)BYBYB^5BaHBdZBdZBgmB`BBW
BR�BQ�B\)BcTBXBVBXB_;B`BBjBt�Bt�Bt�Bu�Bv�Bz�Bx�Bw�Bx�By�Bz�Bu�Bp�Bt�Bp�Bn�Bo�Bs�Bq�Bp�Bo�Bm�Bl�Bk�BgmBjBm�Bm�BhsBgmBdZBe`BiyBk�Bp�Bp�Br�Br�Br�Br�Bq�Bq�Bs�Bt�Bu�Bu�Bu�Bt�Bs�Br�Br�Bt�Bt�Bu�Bx�Bz�Bx�Bx�Bs�Bm�BjBgmBffBbNBffB]/BffBq�Bu�Bk�BffBbNB_;B^5B\)B]/B^5BffBffBk�Bn�Bm�Bn�Br�Bv�B|�B|�B}�B}�B~�B� B�B�B�PB�uB��B��B��B��B��B�B�9B�XB�qBB��BƨBɺBɺBĜB�}BǮB��B��B��BɺB��B��B��B�B�yB�B�ZB�`B�mB�fB�TB�HB�ZB�fB�fB�B�B�B�B��B��B	B	B	B		7B	DB	\B	hB	PB	VB	hB	�B	�B	�B	�B	�B	"�B	&�B	&�B	&�B	%�B	!�B	�B	�B	!�B	&�B	'�B	+B	/B	2-B	7LB	7LB	9XB	:^B	?}B	A�B	B�B	G�B	M�B	P�B	Q�B	T�B	ZB	]/B	^5B	`BB	bNB	bNB	e`B	iyB	k�B	n�B	p�B	r�B	r�B	t�B	v�B	z�B	� B	�B	�B	�+B	�7B	�DB	�JB	�PB	�VB	�bB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�!B	�-B	�-B	�3B	�?B	�?B	�FB	�RB	�qB	�qB	�qB	�qB	�wB	��B	��B	��B	��B	B	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�)B	�)B	�)B	�)B	�/B	�5B	�BB	�BB	�HB	�HB	�TB	�TB	�ZB	�fB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	��B
%B
�B
�B
~B
)DB
6`B
<jB
?�B
B�B
I7B
P�B
TB
YeB
]�B
abB
fB
i�B
oiB
sMB
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
[B
ZB
[B
[B
[B
[B
ZB
[B
[B
[B
\B
\B
\B
\B
\B
[B
\B
\B
[B
ZB
\B
\B
]$B
]$B
\B
\B
]$B
ZB
ZB
ZB
ZB
ZB
[B
ZB
\B
\B
\B
]$B
\B
^*B
_0B
a;B
bAB
ilB
�"BW�BT�BhSBmqB~�B�'B��B��B��B��B��B�QBͭB�9B��B��BPB?BEBEBEBKBQBdB_B:BFB5B/B(BRBwBB#B0BBB
BB �B �B��B�BƊBɜB��B��B��B��B��B�mB�B��B��B��B�LB�BifBYBJ�B9JB-B�BdBB
�OB
ʾB
�]B
��B
�SB
o�B
WB
P�B
C�B
6QB
+B
�B
�B
	HB	��B	�VB	��B	��B	��B	�aB	o�B	^XB	PB	=�B	2UB	'B	�B	KB�!B�B��B�
B	;B�B�B�B�B�PB�%B��B��B��B��B��B�GB�B�B��B��B��B��B�hB�VB�>B{ BvBr�Bo�Bn�Bl�Bj�Bf�Bc�B`�B_~B^xB\lBYZBWNBT<BS6BS6BQ*BNBNBS7BVJBY[BY[B\nBUDBLBG�BF�BQ,BXVBMBKBMBT>BUEB_�Bi�Bi�Bi�Bj�Bk�Bo�Bm�Bl�Bm�Bn�Bo�Bj�Be�Bi�Be�Bc�Bd�Bh�Bf�Be�Bd�Bb�Ba�B`�B\tB_�Bb�Bb�B]zB\tBYbBZhB^�B`�Be�Be�Bg�Bg�Bg�Bg�Bf�Bf�Bh�Bi�Bj�Bj�Bj�Bi�Bh�Bg�Bg�Bi�Bi�Bj�Bm�Bo�Bm�Bm�Bh�Bb�B_�B\wB[qBWYB[qBR;B[qBf�Bj�B`�B[rBWZBTGBSBBQ6BR<BSBB[rB[rB`�Bc�Bb�Bc�Bg�Bk�Bq�Bq�Br�Br�BtBuBwBz*B�ZB�B��B��B��B��B��B�B�@B�^B�wB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�|B߂B�^B�dB�qB�jB�XB�LB�^B�jB�jB��B�B�B�B��B��B�B�B�!B�8B	 EB	]B	iB	QB	WB	iB	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 B	$B	'+B	,JB	,JB	.VB	/\B	4{B	6�B	7�B	<�B	B�B	E�B	F�B	I�B	OB	R*B	S0B	U=B	WIB	WIB	Z[B	^sB	`B	c�B	e�B	g�B	g�B	i�B	k�B	o�B	t�B	x
B	zB	|#B	~/B	�<B	�BB	�GB	�MB	�YB	�YB	�_B	�rB	�xB	�xB	�~B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�"B	�(B	�4B	�4B	�;B	�FB	�eB	�eB	�eB	�eB	�kB	�wB	�wB	�wB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�	B	�B	�B	�B	�B	�B	�!B	�'B	�4B	�4B	�:B	�:B	�EB	�EB	�KB	�WB	�^B	�dB	�jB	�jB	�jB	�pB	�vB	�G�O�B	��B	�B	��B
�B
lB
1B
+MB
1VB
4�B
7{B
>#B
E�B
H�B
NPB
R�B
VMB
[B
^�B
dTB
h7B
m�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.011(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144222022020411442220220204114422  AO  ARCAADJP                                                                    20200619170921    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170921  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170921  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114422  IP                  G�O�G�O�G�O�                