CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-05-09T19:17:19Z AOML 3.0 creation; 2016-08-07T21:51:17Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150509191719  20160807145117  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               3A   AO  5287_9017_051                   2C  D   APEX                            6529                            072314                          846 @�OYu1   @�OZ @0�$�/��d�� ě�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    3A   B   B   @�  @�33A   A   A@  A`  A���A���A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD�3D�S3D��3D���D�fD�FfD���D���D�fD�C3D�|�D�ٚD���D�C3D�vfD�� D�3D�6fD�c3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�ff@љ�A33A'33AG33Ag33A�fgA�fgA�fgA���AÙ�Aә�A㙚A�B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��B��fB��3B��fB��fB��fB��fB��fB��fB��fB��fB��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC s3Cs3Cs3Cs3Cs3C
s3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3C s3C"s3C$s3C&s3C(s3C*s3C,s3C.s3C0s3C2s3C4s3C6s3C8s3C:s3C<s3C>s3C@s3CBs3CDs3CFs3CHs3CJs3CLs3CNs3CPs3CRs3CTs3CVs3CXs3CZs3C\s3C^s3C`s3Cbs3Cds3Cfs3Chs3Cjs3Cls3Cns3Cps3Crs3Cts3Cvs3Cxs3Czs3C|s3C~s3C�9�C�9�C�,�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�FgC�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�gD�D��D�D��D�D��D�D��D�D��D�D��D�D�gD�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�gDy�3D�!�D�a�D���D�� D�$�D�T�D�� D��3D�$�D�Q�D��3D�� D� D�Q�Dڄ�D��fD�!�D�D�D�q�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��A��A��A��A� �A� �A�"�A�&�A�&�A�/A�1'A�5?A�9XA�/A�+A�1'A�1'A�1'A�/A�5?A�33A�1'A��A�bA�oA�oA�O�A�\)A���AҲ-A�^5A��/A�oA�S�A���A�ffA���A�`BA��A�dZA��A��yA�-AǾwA���A�/Aå�A��A��#A��;A�ƨA��^A�&�A�ZA�?}A��mA��FA�9XA�ZA���A�;dA�r�A��\A�9XA�9XA�5?A���A�ƨA��A���A�-A�|�A�dZA��A�bA�S�A��PA���A�dZA�1A�=qA�1'A��A��A�v�A�
=A��FA���A��AXA|��Az��AyO�AydZAv��Ar�!ApM�An��Ak�wAg7LAe��AeoAc?}AaO�A`{A_`BA^Q�A\�\AV�!AS��AN�AK�AH��AF5?AA��A>�/A<bNA<bA;�A;��A:�DA9G�A7K�A3��A1O�A0~�A.��A+�FA*E�A(��A($�A'��A'G�A'VA&�A&�A%�PA$�!A#�A"A�A��AA��A�AhsA{A�#A?}A�A�A&�At�AjA�/A��A~�A��AO�AȴA�hA?}A��AC�A
��A
  A	p�AĜAp�AZA�A��Al�AG�AC�A�A�yAr�AA;dA�A+A ��A ffA Q�@���@��@�J@��@�?}@�^5@��;@��#@�@��;@���@�7L@�(�@�n�@�%@ꟾ@��
@�J@㕁@��@�&�@��D@� �@��;@�ƨ@߶F@�t�@�;d@�
=@��H@��@�33@�@���@�b@�ƨ@�l�@��y@�@�G�@���@�Z@��m@��@��#@ԣ�@�t�@ҏ\@�{@љ�@�G�@Ь@ϕ�@���@�n�@�$�@��@��@��#@�`B@�9X@�dZ@ʗ�@�$�@��#@Ɂ@���@�Q�@�A�@�Q�@�A�@�1'@�9X@�Z@�1@�t�@ǥ�@ǅ@�"�@��@�ȴ@�M�@�-@��@�X@���@�(�@þw@�t�@��@�^5@�-@���@�p�@�&�@��`@���@��@��j@�1'@��
@��P@�+@��@���@�@���@�p�@�%@���@���@���@��D@��@�bN@���@���@��@�33@��y@���@��\@�^5@���@�G�@��/@���@��9@�Z@��;@�|�@�"�@��R@��+@�E�@��@�x�@�V@�I�@��@�|�@�+@�@���@���@�ff@��@��@�@�7L@���@�z�@�A�@�(�@�  @��
@���@�33@�@�ȴ@���@��+@���@��h@�`B@��@���@��D@�A�@��@�ƨ@�K�@��@���@�n�@�^5@�$�@�$�@�@��7@�%@��`@��u@�r�@�Q�@�I�@�9X@�b@�ƨ@�dZ@�@��y@���@�n�@�M�@�5?@��@�@��@��@��@��@��T@��^@��@�7L@��9@�bN@�1@�ƨ@�\)@�
=@���@�V@��@���@�O�@�Ĝ@��D@��u@�bN@��@��
@��
@��P@�+@�@���@�-@���@���@�/@��u@�I�@�9X@���@��@�ff@���@���@���@�?}@��@��@�Z@�Z@�1@���@�dZ@���@��T@�G�@���@���@�Ĝ@��9@���@���@��u@��@�(�@��;@���@���@���@�\)@�;d@��R@�~�@�^5@�5?@���@��@�J@��@��7@�7L@���@��/@��u@�1'@��@��F@���@���@��P@�+@��@��H@��!@�J@���@���@�J@��^@��@��j@���@�r�@�9X@�@�G�@��@x�`@p1'@fV@]p�@U/@L��@D��@<j@2�\@,z�@'
=@!7L@��@X@�@Ĝ@$�@	X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��A��A��A��A��A��A��A��A� �A� �A�"�A�&�A�&�A�/A�1'A�5?A�9XA�/A�+A�1'A�1'A�1'A�/A�5?A�33A�1'A��A�bA�oA�oA�O�A�\)A���AҲ-A�^5A��/A�oA�S�A���A�ffA���A�`BA��A�dZA��A��yA�-AǾwA���A�/Aå�A��A��#A��;A�ƨA��^A�&�A�ZA�?}A��mA��FA�9XA�ZA���A�;dA�r�A��\A�9XA�9XA�5?A���A�ƨA��A���A�-A�|�A�dZA��A�bA�S�A��PA���A�dZA�1A�=qA�1'A��A��A�v�A�
=A��FA���A��AXA|��Az��AyO�AydZAv��Ar�!ApM�An��Ak�wAg7LAe��AeoAc?}AaO�A`{A_`BA^Q�A\�\AV�!AS��AN�AK�AH��AF5?AA��A>�/A<bNA<bA;�A;��A:�DA9G�A7K�A3��A1O�A0~�A.��A+�FA*E�A(��A($�A'��A'G�A'VA&�A&�A%�PA$�!A#�A"A�A��AA��A�AhsA{A�#A?}A�A�A&�At�AjA�/A��A~�A��AO�AȴA�hA?}A��AC�A
��A
  A	p�AĜAp�AZA�A��Al�AG�AC�A�A�yAr�AA;dA�A+A ��A ffA Q�@���@��@�J@��@�?}@�^5@��;@��#@�@��;@���@�7L@�(�@�n�@�%@ꟾ@��
@�J@㕁@��@�&�@��D@� �@��;@�ƨ@߶F@�t�@�;d@�
=@��H@��@�33@�@���@�b@�ƨ@�l�@��y@�@�G�@���@�Z@��m@��@��#@ԣ�@�t�@ҏ\@�{@љ�@�G�@Ь@ϕ�@���@�n�@�$�@��@��@��#@�`B@�9X@�dZ@ʗ�@�$�@��#@Ɂ@���@�Q�@�A�@�Q�@�A�@�1'@�9X@�Z@�1@�t�@ǥ�@ǅ@�"�@��@�ȴ@�M�@�-@��@�X@���@�(�@þw@�t�@��@�^5@�-@���@�p�@�&�@��`@���@��@��j@�1'@��
@��P@�+@��@���@�@���@�p�@�%@���@���@���@��D@��@�bN@���@���@��@�33@��y@���@��\@�^5@���@�G�@��/@���@��9@�Z@��;@�|�@�"�@��R@��+@�E�@��@�x�@�V@�I�@��@�|�@�+@�@���@���@�ff@��@��@�@�7L@���@�z�@�A�@�(�@�  @��
@���@�33@�@�ȴ@���@��+@���@��h@�`B@��@���@��D@�A�@��@�ƨ@�K�@��@���@�n�@�^5@�$�@�$�@�@��7@�%@��`@��u@�r�@�Q�@�I�@�9X@�b@�ƨ@�dZ@�@��y@���@�n�@�M�@�5?@��@�@��@��@��@��@��T@��^@��@�7L@��9@�bN@�1@�ƨ@�\)@�
=@���@�V@��@���@�O�@�Ĝ@��D@��u@�bN@��@��
@��
@��P@�+@�@���@�-@���@���@�/@��u@�I�@�9X@���@��@�ff@���@���@���@�?}@��@��@�Z@�Z@�1@���@�dZ@���@��T@�G�@���@���@�Ĝ@��9@���@���@��u@��@�(�@��;@���@���@���@�\)@�;d@��R@�~�@�^5@�5?@���@��@�J@��@��7@�7L@���@��/@��u@�1'@��@��F@���@���@��P@�+@��@��H@��!@�J@���@���@�J@��^@��@��j@���@�r�G�O�@�@�G�@��@x�`@p1'@fV@]p�@U/@L��@D��@<j@2�\@,z�@'
=@!7L@��@X@�@Ĝ@$�@	X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	jB	jB	jB	jB	jB	jB	jB	jB	jB	jB	jB	jB	jB	jB	jB	k�B	jB	l�B	m�B	k�B	k�B	k�B	l�B	k�B	l�B	l�B	q�B	u�B	v�B	w�B	��B	��B	�yB	��B
-B
K�B
jB
�oB
��B
�;B�B33B��B�B�!B�LB�BBVB�B�B�B$�B�B1B1BDBDB
=B1B  B  B��B��BJB33B1'B)�B1'B)�B�B�BDB��B�#B��B�Bx�BN�BG�BM�BF�B33B
=B
��B
�sB
��B
�B
�oB
\)B
�B
�B
,B
�B	�B	�HB	�TB	�B	�BB	ȴB	�dB	�B	��B	�B	y�B	r�B	gmB	\)B	T�B	N�B	F�B	6FB	�B	B�yB�TB�B�B�B�B�;B�BB�BB�NB�NB�5B��B�}B�3B�B�B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B�bB�\B�VB��B��B��B�hB�VB�=B�B�B�B�B�B� B�B�B�B�B�B�DB��B��B�B�B�B�!B�!B�!B�!B�!B�B�B�B�B�B�!B�3B�XB�^B�dB�^B�^B�RB�FB�B�-B�XB�dB�wB��BB�}B�qB�XB�3B�!B�B�-B�9B�XB�qB��BBÖBĜBĜBƨBɺB��B��B�B�ZB�mB�sB�B�B�B�B�B�B��B��B��B�B�B��B��B��B��B��B��B	B	B	%B	1B	+B	+B		7B	bB	{B	�B	�B	�B	�B	"�B	)�B	,B	.B	33B	8RB	<jB	>wB	?}B	A�B	H�B	L�B	L�B	N�B	O�B	O�B	P�B	R�B	VB	XB	[#B	\)B	^5B	aHB	dZB	ffB	hsB	k�B	o�B	q�B	s�B	v�B	y�B	}�B	~�B	�B	�B	�B	�B	�1B	�=B	�=B	�JB	�\B	�\B	�\B	�\B	�\B	�bB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�-B	�3B	�FB	�RB	�^B	�dB	�jB	�qB	�wB	�}B	�}B	��B	ÖB	ĜB	ĜB	ŢB	ŢB	ŢB	ƨB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�;B	�BB	�HB	�NB	�TB	�TB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
%B
%B
+B
+B
%B
%B
+B
1B
1B
	7B

=B

=B
	7B
	7B
1B
1B
	7B
DB
bB
bB
\B
bB
bB
bB
bB
uB
�B
�B
(�B
.B
8RB
<jB
B�B
H�B
M�B
VB
]/B
bNB
ffB
l�B
o�B
r�B
v�B
y�B
|�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	jkB	jkB	jkB	jmB	jkB	jmB	jmB	jmB	jkB	jmB	jjB	jlB	jjB	jjB	jlB	kqB	jlB	ltB	m}B	koB	krB	koB	luB	knB	lxB	lxB	q�B	u�B	v�B	w�B	��B	��B	�`B	��B
,�B
K�B
jaB
�NB
��B
�BrB3B��B��B��B�+B��B�B0BnBbBmB$�B�B
B	BBB
B	B��B��B��B��B!B3	B0�B)�B0�B)�B~BeBB��B��B�[B��Bx�BN�BG�BM�BF}B3	B
B
��B
�IB
��B
��B
�DB
\B
_B
sB
+�B
fB	�B	�#B	�/B	�B	�B	ȐB	�AB	��B	��B	��B	y�B	r�B	gKB	\B	T�B	N�B	F�B	6$B	pB	�B�XB�6B��B��B��B��B�B�%B�#B�-B�0B�BϾB�^B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�mB�`B�BB�<B�7B�tB��B��B�JB�4B�B��B��B��B��B��B�B��B��B��B��B��B�$B�fB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�2B�9B�@B�8B�:B�+B�B��B�B�3B�?B�RB�_B�iB�XB�LB�2B�B��B��B�B�B�/B�IB�\B�hB�qB�wB�vB�BɓBʜBпB��B�4B�CB�JB�WB�_B�xB�B�B�B��B��B��B�B�B��B��B��B��B��B��B	�B	�B	�B	B	 B	�B		B	7B	PB	nB	|B	�B	�B	"�B	)�B	+�B	-�B	3B	8'B	<>B	>JB	?OB	A]B	H�B	L�B	L�B	N�B	O�B	O�B	P�B	R�B	U�B	W�B	Z�B	[�B	^B	aB	d,B	f5B	hEB	kWB	opB	qzB	s�B	v�B	y�B	}�B	~�B	��B	��B	��B	��B	�B	�B	�B	�B	�.B	�,B	�.B	�,B	�,B	�2B	�=B	�LB	�JB	�TB	�\B	�cB	�jB	�kB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�1B	�6B	�<B	�?B	�FB	�KB	�JB	�ZB	�dB	�iB	�kB	�pB	�qB	�qB	�wB	ȃB	ȁB	ʎB	˕B	˔B	ΦB	ϩB	еB	ѹB	��B	ҿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�"B	�#B	�(B	�'B	�-B	�-B	�+B	�1B	�3B	�3B	�1B	�3B	�4B	�5B	�;B	�?B	�FB	�MB	�KB	�KB	�LB	�YB	�VB	�]B	�eB	�iB	�uB	�tB	�~B	�|B	�vB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B

	B

	B
	B
	B
�B
�B
	B
B
-B
.B
(B
+B
+B
,G�O�B
>B
jB
�B
(�B
-�B
8B
<3B
B[B
H}B
M�B
U�B
\�B
bB
f0B
lSB
ogB
r{B
v�B
y�B
|�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.45 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451172016080714511720160807145117  AO  ARCAADJP                                                                    20150509191719    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150509191719  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150509191719  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145117  IP                  G�O�G�O�G�O�                