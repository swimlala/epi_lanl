CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-05-02T09:16:09Z AOML 3.0 creation; 2016-08-07T21:51:29Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160502091609  20160807145129  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               wA   AO  5287_9017_119                   2C  D   APEX                            6529                            072314                          846 @ש��@1   @ש�m��@0�O�;dZ�d���l�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    wA   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3DyffD��3D�C3D�c3D�ɚD�	�D�VfD�vfD�ٚD��D�fD���DǼ�D��fD�9�DچfD�ɚD��fD�I�D�i�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�ffA33A'33AG33Ag33A���A���A���A���AÙ�Aә�A㙚A�B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC s3Cs3Cs3Cs3Cs3C
s3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3C s3C"s3C$s3C&Y�C(s3C*s3C,s3C.s3C0s3C2s3C4s3C6s3C8s3C:s3C<s3C>s3C@s3CBs3CDs3CFs3CHs3CJs3CLs3CNs3CPs3CRs3CTs3CVs3CXs3CZs3C\s3C^s3C`s3Cbs3Cds3Cfs3Chs3Cjs3Cls3Cns3Cps3Crs3Cts3Cvs3Cxs3Czs3C|s3C~s3C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�,�C�,�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�,�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1�3D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do�3Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt� Dy�3D���D�Q�D�q�D�� D� D�d�D���D�� D�3D��D��3D��3D��D�H Dڔ�D�� D��D�X D�x D�љ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ffA�l�A�bNA�^5A�K�A�;dA�$�A�VA�VA�A���A��A��/A���A�ȴA�ȴA�ƨA�ĜA���A���A�A���A���A���AӾwA���A���AӾwAӮA�"�A�+A�Aѕ�Aщ7A�z�A�VA���A�E�A�+A�A��;Aϟ�AσA�l�A�^5A�O�A�=qA�"�A��;A�l�A��Aͩ�A��
A�oA���A˅A�l�A��A��A�A�ffA�$�A���AǛ�A�v�A��
A�~�A�~�A�t�A��A�`BA��!A�XA��
A�I�A���A��A�ffA��A��A�"�A�A�
=A�{A���A��+A�5?A�+A�I�A�ĜA��#A�hsA�;dA�bA���A��A���A��A�Q�A��!A���A���A�;dA�z�A��A���A���A���A��A{�#AxbAu�mArn�Ao�^Am�#AkS�Ajv�Ah1Ad�A`VA[?}AWASK�AP�AMx�AJJAH��AG�-AF=qAD�HACl�AA�^A>jA<9XA:~�A8�A5�#A4Q�A2�A21'A1��A0�jA0bA.M�A+�wA(�A'7LA&�A&bA%G�A#�PA"�yA"5?A!O�AjA�7AVA�9A  A?}A�AffAdZA~�AƨAĜAbNA5?A��A�A%AoA�9AE�AbA9XA��Ax�A
�!A	�mA	G�A	7LA	7LA�DA��A7LA�`A$�A�^A`BA�A��A�A"�A
=A ��A
=A�A ĜA Q�@���@�o@�=q@�J@��h@�C�@���@�S�@��+@�@�?}@�1'@�;d@�J@�@�O�@��`@�b@@�@��/@�(�@�ȴ@��y@�^5@�$�@�`B@�r�@㕁@�S�@�;d@�"�@��H@�!@�~�@�^5@�$�@ᙚ@�G�@�%@���@�r�@�1'@�1'@��D@���@ߍP@޸R@��@�`B@��@٩�@�%@� �@�  @��@��y@�@պ^@ղ-@ղ-@�x�@���@Լj@��@��;@�;d@��@���@Ѳ-@�p�@�?}@���@϶F@Η�@��@�Ĝ@�Q�@�b@�  @˕�@�+@�E�@ɲ-@��@���@�S�@�@�~�@�J@ř�@��@��`@���@ě�@�z�@�Q�@ÍP@�~�@�=q@�@���@��@�`B@�%@�Ĝ@�1'@�ȴ@��@�O�@�V@���@���@��/@���@���@��u@�Q�@� �@�b@���@��;@�ƨ@��F@���@�|�@�"�@�M�@�`B@�V@��j@���@��D@�r�@� �@��
@�l�@�33@��!@��+@�ff@�5?@�$�@�O�@���@�A�@�1@��m@�
=@���@�E�@�@��@��#@��h@�p�@�hs@�O�@�?}@�7L@�/@�V@�%@���@��j@�Q�@�b@��
@�dZ@��@�~�@�5?@�@���@��@���@���@���@��@�Ĝ@�G�@��^@���@���@��^@�x�@�7L@��j@�Q�@�b@��@��H@�J@��@��#@���@�&�@��u@�(�@���@��@��@��F@��@�t�@�C�@�+@��y@���@��^@��@�`B@�?}@�V@��9@�Z@���@��w@���@��P@�+@��H@���@��T@���@�X@��@�%@�Ĝ@�z�@��D@�z�@��
@��F@��F@�t�@�S�@�C�@�o@��!@�=q@�J@��#@��7@�G�@��`@�j@��@�ƨ@��P@�K�@��y@���@�^5@�=q@�$�@�J@���@���@�?}@���@���@���@��@��u@�r�@��@�C�@��@���@�@�G�@��@�%@��@��/@���@���@��u@��D@�Q�@�(�@�1@���@��@��+@�M�@�5?@��9@�V@���@{@r��@i�@_�P@W�P@L��@I��@@�`@9%@3��@.{@'|�@ �`@��@ �@�h@ƨ@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�ffA�l�A�bNA�^5A�K�A�;dA�$�A�VA�VA�A���A��A��/A���A�ȴA�ȴA�ƨA�ĜA���A���A�A���A���A���AӾwA���A���AӾwAӮA�"�A�+A�Aѕ�Aщ7A�z�A�VA���A�E�A�+A�A��;Aϟ�AσA�l�A�^5A�O�A�=qA�"�A��;A�l�A��Aͩ�A��
A�oA���A˅A�l�A��A��A�A�ffA�$�A���AǛ�A�v�A��
A�~�A�~�A�t�A��A�`BA��!A�XA��
A�I�A���A��A�ffA��A��A�"�A�A�
=A�{A���A��+A�5?A�+A�I�A�ĜA��#A�hsA�;dA�bA���A��A���A��A�Q�A��!A���A���A�;dA�z�A��A���A���A���A��A{�#AxbAu�mArn�Ao�^Am�#AkS�Ajv�Ah1Ad�A`VA[?}AWASK�AP�AMx�AJJAH��AG�-AF=qAD�HACl�AA�^A>jA<9XA:~�A8�A5�#A4Q�A2�A21'A1��A0�jA0bA.M�A+�wA(�A'7LA&�A&bA%G�A#�PA"�yA"5?A!O�AjA�7AVA�9A  A?}A�AffAdZA~�AƨAĜAbNA5?A��A�A%AoA�9AE�AbA9XA��Ax�A
�!A	�mA	G�A	7LA	7LA�DA��A7LA�`A$�A�^A`BA�A��A�A"�A
=A ��A
=A�A ĜA Q�@���@�o@�=q@�J@��h@�C�@���@�S�@��+@�@�?}@�1'@�;d@�J@�@�O�@��`@�b@@�@��/@�(�@�ȴ@��y@�^5@�$�@�`B@�r�@㕁@�S�@�;d@�"�@��H@�!@�~�@�^5@�$�@ᙚ@�G�@�%@���@�r�@�1'@�1'@��D@���@ߍP@޸R@��@�`B@��@٩�@�%@� �@�  @��@��y@�@պ^@ղ-@ղ-@�x�@���@Լj@��@��;@�;d@��@���@Ѳ-@�p�@�?}@���@϶F@Η�@��@�Ĝ@�Q�@�b@�  @˕�@�+@�E�@ɲ-@��@���@�S�@�@�~�@�J@ř�@��@��`@���@ě�@�z�@�Q�@ÍP@�~�@�=q@�@���@��@�`B@�%@�Ĝ@�1'@�ȴ@��@�O�@�V@���@���@��/@���@���@��u@�Q�@� �@�b@���@��;@�ƨ@��F@���@�|�@�"�@�M�@�`B@�V@��j@���@��D@�r�@� �@��
@�l�@�33@��!@��+@�ff@�5?@�$�@�O�@���@�A�@�1@��m@�
=@���@�E�@�@��@��#@��h@�p�@�hs@�O�@�?}@�7L@�/@�V@�%@���@��j@�Q�@�b@��
@�dZ@��@�~�@�5?@�@���@��@���@���@���@��@�Ĝ@�G�@��^@���@���@��^@�x�@�7L@��j@�Q�@�b@��@��H@�J@��@��#@���@�&�@��u@�(�@���@��@��@��F@��@�t�@�C�@�+@��y@���@��^@��@�`B@�?}@�V@��9@�Z@���@��w@���@��P@�+@��H@���@��T@���@�X@��@�%@�Ĝ@�z�@��D@�z�@��
@��F@��F@�t�@�S�@�C�@�o@��!@�=q@�J@��#@��7@�G�@��`@�j@��@�ƨ@��P@�K�@��y@���@�^5@�=q@�$�@�J@���@���@�?}@���@���@���@��@��u@�r�@��@�C�@��@���@�@�G�@��@�%@��@��/@���@���@��u@��D@�Q�@�(�@�1@���@��@��+@�M�G�O�@��9@�V@���@{@r��@i�@_�P@W�P@L��@I��@@�`@9%@3��@.{@'|�@ �`@��@ �@�h@ƨ@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�TB
�TB
�TB
�TB
�HB
�BB
�;B
�/B
�/B
�/B
�/B
�/B
�5B
�5B
�;B
�BB
�BB
�HB
�HB
�HB
�NB
�TB
�`B
�fB
�mB
�sB
�yB
�B
�B
�B
��B
��B
��B
��B
��B
��B
�B
�NB
�mB
�yB
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B%B
=BDBPB�B2-B9XB@�BP�Be`BffBffBw�BĜBVB�B�B>wBq�B�B�+B�JB�oB�hB�PB�Bz�Br�Bp�BjBK�B-B'�B.B(�B�BB��B��B��B�jB�FB�3B�'B��Bz�BT�B1'B�B
�B
��B
ɺB
�XB
�B
��B
�+B
iyB
@�B
'�B
�B
  B	�B	�HB	��B	ȴB	�LB	��B	~�B	`BB	I�B	7LB	,B	 �B	uB	JB	+B	B��B��B�B�fB�BB�B�B��B��B��B��BȴBƨBÖB��B�qB�jB�dB�dB�^B�RB�XB�XB�RB�FB�'B�!B�B�B��B��B��B��B��B��B�3B�FB�RB�XB�LB�3B�!B�3B�?B�RB��BĜB��B��B�)B�NB�mB�mB�mB�fB�mB�mB�mB�B��B��B�B�sB�ZB�`B�mB�yB�B�B�B�B��B��B	%B	1B		7B	\B	oB	�B	�B	�B	�B	!�B	&�B	.B	0!B	1'B	2-B	33B	33B	49B	33B	49B	7LB	>wB	@�B	A�B	C�B	D�B	G�B	H�B	H�B	H�B	I�B	J�B	K�B	K�B	L�B	N�B	O�B	P�B	Q�B	R�B	S�B	VB	]/B	aHB	ffB	hsB	iyB	iyB	n�B	r�B	t�B	x�B	x�B	x�B	{�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�DB	�JB	�JB	�PB	�VB	�VB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�?B	�FB	�FB	�FB	�FB	�FB	�FB	�LB	�RB	�jB	�wB	��B	��B	��B	��B	��B	��B	B	B	B	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�;B	�;B	�BB	�HB	�HB	�NB	�ZB	�`B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
%B
%B
%B
B
%B
%B
%B
+B
+B
+B
+B
+B
+B
+B
	7B
	7B
	7B

=B

=B

=B

=B

=B
DB
JB
JB
VB
VB
\B
\B
\B
\B
\B
bB
bB
bB
bB
hB
bB
hB
hB
oB
uB
uB
{B
�B
!�B
&�B
/B
49B
<jB
A�B
J�B
L�B
R�B
YB
]/B
aHB
iyB
n�B
r�B
v�B
y�B
{�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
�4B
�2B
�3B
�3B
�%B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�"B
�'B
�)B
�*B
�-B
�1B
�>B
�DB
�OB
�SB
�ZB
�\B
�eB
�B
��B
��B
��B
��B
��B
��B
�_B
�)B
�MB
�WB
��B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��BB
BB,B�B2
B94B@^BP�Be=Bf@BfABw�B�uB-BaB�B>RBq�B��B�B�%B�KB�CB�)B��Bz�Br�Bp{BjXBK�B,�B'�B-�B(�ByB �B��B̢B�_B�?B�B�B��B�vBz�BT�B0�B�B
�~B
��B
ɒB
�-B
��B
��B
�B
iPB
@[B
'�B
gB	��B	�rB	�!B	��B	ȏB	�&B	��B	~�B	`B	I�B	7)B	+�B	 �B	VB	*B		B	 �B��B��B�xB�FB�"B� B��B��BϽBͳB˦BȕBƇB�sB�cB�OB�IB�CB�DB�?B�0B�6B�7B�0B�$B�B� B��B��B��B��B��B��B��B��B�B�"B�/B�5B�)B�B��B�B�B�-B�_B�{BˢB��B�B�*B�GB�HB�GB�AB�IB�HB�GB�eB��B��B�B�LB�3B�;B�EB�TB�XB�_B�yB�B��B��B	�B	B		B	5B	EB	jB	�B	�B	�B	!�B	&�B	-�B	/�B	0�B	2B	3	B	3
B	4B	3	B	4B	7"B	>IB	@WB	A]B	CkB	DqB	G�B	H�B	H�B	H�B	I�B	J�B	K�B	K�B	L�B	N�B	O�B	P�B	Q�B	R�B	S�B	U�B	]B	aB	f9B	hDB	iLB	iJB	nlB	r�B	t�B	x�B	x�B	x�B	{�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�'B	�'B	�8B	�EB	�OB	�XB	�XB	�WB	�VB	�_B	�fB	�xB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�9B	�FB	�QB	�YB	�YB	�YB	�XB	�XB	�^B	�\B	�`B	�dB	�eB	�eB	�lB	�lB	�lB	�kB	�kB	�qB	�|B	ȂB	ɇB	ʐB	ʑB	ʓB	ʏB	˔B	˕B	̛B	̜B	͢B	ͣB	͢B	͢B	͢B	ϪB	вB	ѼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	�B	�)B	�.B	�;B	�PB	�dB	�dB	�kB	�rB	�qB	�jB	�fB	�^B	�TB	�FB	�LB	�QB	�RB	�RB	�PB	�RB	�PB	�ZB	�rB	�{B	�zB	�|B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	B
	B

B

	B

B

	B

B
B
B
B
B
$B
(B
(B
)B
'B
&B
,B
,B
/B
-B
4B
.B
3B
4B
9B
CG�O�B
FB
sB
!�B
&�B
.�B
4B
<3B
ARB
J�B
L�B
R�B
X�B
\�B
aB
i@B
ndB
r|B
v�B
y�B
{�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.45 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451292016080714512920160807145129  AO  ARCAADJP                                                                    20160502091609    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160502091609  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160502091609  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145129  IP                  G�O�G�O�G�O�                