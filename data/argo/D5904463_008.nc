CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:52Z AOML 3.0 creation; 2016-08-07T22:44:58Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221452  20160807154458  5904463 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5288_9005_008                   2C  D   APEX                            6530                            072314                          846 @�:���1   @�;"!��@(�^5?|��cm��l�D1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B��B  B33B��B*ffB-��B7��B@  BHffBP  BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C�C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�DyS3D��3D�\�D��fD��3D�	�D�@ D�|�D��fD��D�FfD��3D��fD�3D�6fD�y�D��D�  D�,�D�i�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�ffA33A'33AG33Ag33A���A���A���A���A���Aә�A㙚A�B��B
��B��B  B!fgB,33B/fgB9fgBA��BJ33BQ��BYfgBa��Bi��Bq��By��B��fB��fB��fB��fB��fB��fB��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��C s3Cs3Cs3Cs3Cs3C
s3Cs3Cs3Cs3Cs3Cs3Cs3C��Cs3Cs3Cs3C s3C"s3C$s3C&s3C(s3C*s3C,s3C.Y�C0s3C2s3C4s3C6s3C8s3C:s3C<s3C>s3C@s3CBs3CDs3CFs3CHs3CJs3CLs3CNs3CPs3CRs3CTs3CVs3CXs3CZs3C\s3C^s3C`s3Cbs3Cds3Cfs3Chs3Cjs3Cls3Cns3Cps3Crs3Cts3Cvs3Cxs3Czs3C|s3C~s3C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�FgC�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�gDyp D��D�k3D���D��D� D�NfD��3D���D�( D�T�D���D���D��D�D�Dڈ D��3D�fD�;3D�x D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A޾wA���A�ĜA���A���A���A��
A��
A��;A��yA��A��A��A��A��`A���A�z�Aݝ�A�|�A�(�A�~�A˩�Aǡ�A�~�A�1'A�`BA�I�A�I�A�&�A�^5A�ZA�ZA��A��A�r�A��^A��PA�A�VA��HA�A�O�A�r�A�x�A��!A���A��wA��^A�\)A��-A��;A�oA��A�$�A���A���A��;A��A���A�I�A���A�ffA�O�A�&�A���A�E�A���A���A�v�A�hAy�7As?}Am��Ai�
Ae�Ab��A`��A[��AY��AU�;AT��AT$�AS��AL��AE�TADA�A<��A:9XA9��A8��A7��A7C�A7�A85?A7��A6�HA6I�A4�`A4��A4^5A3�A3�PA2$�A1��A0�A0I�A/��A.��A+�A+p�A+C�A+;dA+VA*��A)\)A(bNA(=qA(1A'XA&A�A%�;A%\)A$�9A$�+A$E�A#�A#|�A#&�A"r�A"A!��A!�7A!�A ��A (�A�hA�A�uAbA��A��A9XAx�AK�A�A��AffA�A�A�!A{AhsAG�A"�A�A�RAv�A �A�;A��AO�A�A��A��AM�A�#A��AhsAXAO�AC�A?}A;dA?}AK�AS�AG�A�A�HAz�AjA�9A33AK�A7LA"�A�A�A��AffA�A�FA�A33A��AVA��A��A�AS�A�`A��A9XA�A7LA/A�A�RA��A5?A��A�wA��Al�A;dA
��A
�DA
ZA
�A	�A	�PA	��A	p�A	G�A	33A�HA�uAbNA �A�AC�A�`A��A�AM�A�A�A�A�RA^5A�A��A�A\)A�A�A��AffA�AA�A �`A A�A @��@�C�@���@��^@�V@�(�@��@��@���@���@���@�"�@�v�@��h@�Ĝ@�  @�"�@�R@�^5@�@�7@��@��@��@�$�@�V@���@�j@�F@�t�@�@��@�^5@��@��@���@��;@�S�@�R@���@���@�z�@�(�@���@��m@�w@�K�@�\@�$�@�{@�&�@�j@���@�|�@�C�@�
=@ާ�@�J@���@��@�Z@۾w@��y@ٲ-@���@�j@׮@�\)@���@�v�@�$�@��#@�X@Լj@Ӯ@���@ҸR@җ�@�n�@�=q@��#@щ7@Л�@�t�@�
=@θR@�v�@�M�@���@ͩ�@̬@�C�@��T@ɉ7@�hs@��@�b@�"�@��y@ƸR@�E�@�X@���@�Ĝ@�Q�@þw@ÍP@�\)@�;d@�
=@��H@���@�5?@��@��9@��@�1'@���@��y@�~�@�-@��T@���@�p�@��@��j@�r�@� �@��P@�"�@���@���@��+@�n�@�{@�x�@�O�@�/@���@�(�@� �@� �@��
@��y@�E�@��^@�`B@���@��@�r�@��
@���@�~�@�=q@�@��@���@���@�bN@�b@�l�@�n�@�@���@��h@�?}@�%@��D@��m@�ȴ@�~�@�=q@�@���@�7L@�j@� �@���@���@�C�@��@���@�@��-@�x�@�G�@���@�1'@��;@��P@�;d@���@��@��7@�/@�Ĝ@�1@�dZ@��@�n�@��T@���@�hs@�?}@��`@��@��w@�S�@���@�E�@���@�p�@�7L@��@�%@���@��@��;@�l�@�
=@��R@�v�@�=q@�J@��@��7@���@�bN@�9X@�b@��w@��@�S�@��@��@��F@��#@�w@r��@iX@_+@W
=@M�@Bn�@<j@6ȴ@0r�@*��@#o@E�@�7@5?@�9@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A޾wA���A�ĜA���A���A���A��
A��
A��;A��yA��A��A��A��A��`A���A�z�Aݝ�A�|�A�(�A�~�A˩�Aǡ�A�~�A�1'A�`BA�I�A�I�A�&�A�^5A�ZA�ZA��A��A�r�A��^A��PA�A�VA��HA�A�O�A�r�A�x�A��!A���A��wA��^A�\)A��-A��;A�oA��A�$�A���A���A��;A��A���A�I�A���A�ffA�O�A�&�A���A�E�A���A���A�v�A�hAy�7As?}Am��Ai�
Ae�Ab��A`��A[��AY��AU�;AT��AT$�AS��AL��AE�TADA�A<��A:9XA9��A8��A7��A7C�A7�A85?A7��A6�HA6I�A4�`A4��A4^5A3�A3�PA2$�A1��A0�A0I�A/��A.��A+�A+p�A+C�A+;dA+VA*��A)\)A(bNA(=qA(1A'XA&A�A%�;A%\)A$�9A$�+A$E�A#�A#|�A#&�A"r�A"A!��A!�7A!�A ��A (�A�hA�A�uAbA��A��A9XAx�AK�A�A��AffA�A�A�!A{AhsAG�A"�A�A�RAv�A �A�;A��AO�A�A��A��AM�A�#A��AhsAXAO�AC�A?}A;dA?}AK�AS�AG�A�A�HAz�AjA�9A33AK�A7LA"�A�A�A��AffA�A�FA�A33A��AVA��A��A�AS�A�`A��A9XA�A7LA/A�A�RA��A5?A��A�wA��Al�A;dA
��A
�DA
ZA
�A	�A	�PA	��A	p�A	G�A	33A�HA�uAbNA �A�AC�A�`A��A�AM�A�A�A�A�RA^5A�A��A�A\)A�A�A��AffA�AA�A �`A A�A @��@�C�@���@��^@�V@�(�@��@��@���@���@���@�"�@�v�@��h@�Ĝ@�  @�"�@�R@�^5@�@�7@��@��@��@�$�@�V@���@�j@�F@�t�@�@��@�^5@��@��@���@��;@�S�@�R@���@���@�z�@�(�@���@��m@�w@�K�@�\@�$�@�{@�&�@�j@���@�|�@�C�@�
=@ާ�@�J@���@��@�Z@۾w@��y@ٲ-@���@�j@׮@�\)@���@�v�@�$�@��#@�X@Լj@Ӯ@���@ҸR@җ�@�n�@�=q@��#@щ7@Л�@�t�@�
=@θR@�v�@�M�@���@ͩ�@̬@�C�@��T@ɉ7@�hs@��@�b@�"�@��y@ƸR@�E�@�X@���@�Ĝ@�Q�@þw@ÍP@�\)@�;d@�
=@��H@���@�5?@��@��9@��@�1'@���@��y@�~�@�-@��T@���@�p�@��@��j@�r�@� �@��P@�"�@���@���@��+@�n�@�{@�x�@�O�@�/@���@�(�@� �@� �@��
@��y@�E�@��^@�`B@���@��@�r�@��
@���@�~�@�=q@�@��@���@���@�bN@�b@�l�@�n�@�@���@��h@�?}@�%@��D@��m@�ȴ@�~�@�=q@�@���@�7L@�j@� �@���@���@�C�@��@���@�@��-@�x�@�G�@���@�1'@��;@��P@�;d@���@��@��7@�/@�Ĝ@�1@�dZ@��@�n�@��T@���@�hs@�?}@��`@��@��w@�S�@���@�E�@���@�p�@�7L@��@�%@���@��@��;@�l�@�
=@��R@�v�@�=q@�J@��@��7@���@�bN@�9X@�b@��w@��@�S�G�O�@��@��F@��#@�w@r��@iX@_+@W
=@M�@Bn�@<j@6ȴ@0r�@*��@#o@E�@�7@5?@�9@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB1B1B	7BDB\BbBhBbB{B�B�B�B�B�B"�B%�B'�B&�B(�B7LBP�B��B��B�BJB�BM�Bo�Bx�B�DB�uB��B��B��B��B��B��B��B�B�B��B�+Bn�BR�BdZBo�Bp�Bo�Bk�B\)BW
BQ�BO�BG�B<jB5?B#�B
=B��Bz�B_;B8RB
��B
�B
x�B
\)B
?}B
#�B
VB	��B	��B	��B	{�B	`BB	D�B	33B	'�B	�B	hB	�B	�B	%�B	-B	Q�B	R�B	C�B	>wB	I�B	P�B	ZB	r�B	��B	�BB	�B	��B	��B	��B	��B
B
JB
1'B
>wB
5?B
2-B
33B
2-B
1'B
1'B
33B
49B
49B
49B
49B
6FB
=qB
>wB
B�B
C�B
A�B
E�B
E�B
H�B
L�B
P�B
T�B
T�B
XB
ZB
^5B
_;B
aHB
dZB
dZB
ffB
iyB
gmB
e`B
aHB
`BB
^5B
\)B
YB
T�B
S�B
Q�B
O�B
M�B
J�B
H�B
E�B
E�B
G�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
Q�B
R�B
R�B
R�B
Q�B
T�B
ZB
cTB
m�B
o�B
q�B
s�B
u�B
v�B
w�B
u�B
r�B
q�B
p�B
m�B
k�B
hsB
ffB
e`B
ffB
ffB
ffB
gmB
cTB
_;B
]/B
\)B
[#B
YB
YB
W
B
VB
W
B
XB
W
B
VB
R�B
Q�B
P�B
O�B
N�B
N�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
P�B
O�B
O�B
P�B
R�B
Q�B
Q�B
O�B
M�B
M�B
L�B
L�B
K�B
J�B
I�B
H�B
G�B
G�B
F�B
E�B
D�B
D�B
B�B
@�B
>wB
=qB
<jB
<jB
;dB
:^B
:^B
8RB
7LB
6FB
5?B
49B
2-B
1'B
0!B
/B
/B
.B
-B
-B
,B
-B
/B
.B
.B
-B
,B
,B
,B
+B
+B
+B
)�B
)�B
(�B
(�B
(�B
'�B
&�B
&�B
%�B
$�B
$�B
$�B
$�B
$�B
$�B
#�B
#�B
"�B
"�B
!�B
!�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
hB
\B
\B
VB
PB
PB
JB
JB
DB
DB

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
	7B
1B
	7B
	7B
	7B
	7B
DB
JB
JB
JB
JB
JB
DB
JB
PB
JB
JB
PB
PB
PB
PB
VB
PB
PB
VB
PB
PB
PB
PB
PB
PB
JB
JB
PB
PB
PB
PB
PB
VB
VB
bB
hB
hB
hB
hB
bB
bB
bB
bB
\B
bB
hB
hB
oB
oB
hB
oB
hB
oB
oB
oB
oB
oB
oB
{B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
%�B
$�B
+B
1'B
9XB
A�B
F�B
L�B
Q�B
W
B
]/B
aHB
e`B
k�B
n�B
r�B
u�B
x�B
|�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   BBB	B$B=BABHBABZBB}B�B�B�B"�B%�B'�B&�B(�B7*BP�B�dBʛB�B"B�BM�BouBx�B�B�RB��B��B��B��B��B��B��B��B��B�jB�BnmBR�Bd3BouBpwBowBk\B\BV�BQ�BO�BG�B<>B5B#�B
B�ZBz�B_B8+B
��B
��B
x�B
\B
?SB
#�B
2B	��B	δB	��B	{�B	`B	D|B	3B	'�B	nB	FB	dB	�B	%�B	,�B	Q�B	R�B	CoB	>SB	I�B	P�B	Y�B	r�B	��B	�B	�xB	��B	��B	��B	��B
 �B
 B
0�B
>GB
5B
1�B
3B
1�B
0�B
0�B
3B
4
B
4B
4B
4B
6B
=BB
>GB
BbB
ChB
AYB
ErB
EtB
H�B
L�B
P�B
T�B
T�B
W�B
Y�B
^B
_
B
aB
d)B
d(B
f6B
iIB
g>B
e/B
aB
`B
^B
[�B
X�B
T�B
S�B
Q�B
O�B
M�B
J�B
H�B
ErB
EqB
G~B
FxB
GB
GB
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
Q�B
R�B
R�B
R�B
Q�B
T�B
Y�B
c"B
m_B
omB
qzB
s�B
u�B
v�B
w�B
u�B
r�B
qyB
pqB
m`B
kSB
h>B
f4B
e.B
f4B
f4B
f4B
g:B
c$B
_	B
\�B
[�B
Z�B
X�B
X�B
V�B
U�B
V�B
W�B
V�B
U�B
R�B
Q�B
P�B
O�B
N�B
N�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
P�B
O�B
O�B
P�B
R�B
Q�B
Q�B
O�B
M�B
M�B
L�B
L�B
K�B
J�B
I�B
H�B
G~B
G~B
FxB
EoB
DkB
DlB
B^B
@PB
>FB
=AB
<:B
<7B
;1B
:-B
:,B
8 B
7B
6B
5B
4B
1�B
0�B
/�B
.�B
.�B
-�B
,�B
,�B
+�B
,�B
.�B
-�B
-�B
,�B
+�B
+�B
+�B
*�B
*�B
*�B
)�B
)�B
(�B
(�B
(�B
'�B
&�B
&�B
%�B
$�B
$�B
$�B
$�B
$�B
$�B
#�B
#�B
"�B
"�B
!�B
!�B
 �B
 �B
 �B
�B
�B
�B
�B
�B
B
�B
zB
lB
bB
[B
[B
kB
tB
vB
tB
lB
mB
lB
bB
eB
fB
iB
gB
hB
bB
^B
aB
aB
\B
\B
[B
\B
TB
VB
MB
AB
3B
)B
*B
$B
B
B
B
B
B
B

	B

	B


B

B

B

B

	B

B


B

	B
	B
 B
	B
	B
	B
	B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
"B
B
B
!B
B
B
B
 B
B
B
B
B
B
B
B
B
B
#B
$B
2B
3B
6B
5B
5B
0B
/B
/B
1B
)B
/B
3B
5B
;B
<B
5B
:B
5B
<B
<B
<B
:B
;B
9B
GB
LB
JB
HB
MB
NB
MB
TB
TB
QB
SB
VB
TB
RB
XB
XB
YB
^B
`B
^B
gB
eB
eB
kB
lB
lB
jB
lB
kB
kB
sB
rB
sB
wB
~B
}B
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
"�B
"�B
"�B
#�B
#�B
#�G�O�B
$�B
*�B
0�B
9 B
AQB
FpB
L�B
Q�B
V�B
\�B
aB
e)B
kOB
nbB
rxB
u�B
x�B
|�B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.45 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071544582016080715445820160807154458  AO  ARCAADJP                                                                    20150226221452    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221452  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221452  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807154458  IP                  G�O�G�O�G�O�                