CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-01T09:17:23Z AOML 3.0 creation; 2016-08-07T21:51:19Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150701091723  20160807145119  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               =A   AO  5287_9017_061                   2C  D   APEX                            6529                            072314                          846 @�\��9�1   @�\��8��@0�-�d��/��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    =A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C L�C��C�fC�fC  C
  C  C�C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!�fD"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D���D�VfD�� D��3D��D�I�D�33D��3D��D�@ D�s3D�ٚD�fD�L�D�y�D���D��fD�6fD�s3D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�ffA33A'33AG33Ag33A���A���A���A���AÙ�Aә�A㙚A�B��B	��B��B��B!fgB)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B��fB��fB��fB��fB��3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC � C@ CY�CY�Cs3C
s3Cs3C��C��Cs3Cs3Cs3Cs3Cs3Cs3Cs3C s3C"s3C$s3C&s3C(s3C*s3C,s3C.s3C0s3C2s3C4s3C6s3C8s3C:s3C<s3C>s3C@s3CBs3CDs3CFs3CHs3CJs3CLs3CNs3CPs3CRs3CTs3CVs3CXs3CZs3C\s3C^s3C`s3Cbs3Cds3Cfs3Chs3Cjs3Cls3Cns3Cps3Crs3Cts3Cvs3Cxs3Czs3C|s3C~s3C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!�3D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�3Dy��D�� D�d�D��fD�љD�3D�X D�A�D�љD�+3D�NfD���D�� D��D�[3Dڈ D��3D��D�D�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AݓuAݓuA݋DA�p�A�O�A�33A�33A��A�
=A��yAܬAܥ�Aܛ�Aܕ�A܏\A܏\A܋DA܇+A܅A܇+A܃A܃A܃A܁A�|�A�|�A�z�A�x�A�t�A�p�A�jA�hsA�^5A�G�A��A�O�A��A�r�A�A�p�A�;dA�-A�bA�Aԛ�A�x�A�ĜA���A�\)A�bA��yA���AѼjAї�A�oA�I�A�{A͋DA�1'A�ĜA�5?A�JA��A�=qA�r�A���AhA�C�A��A�ƨA�^5A�S�A��jA�v�A�A�$�A��;A���A�O�A��A�{A�`BA�ȴA��#A��uA��+A�1'A��^A��A�/A�ƨA��A�(�A���A�A�~�A�+A���A�XA��mA�v�A�&�A�/A�5?A�ZA���A���A���A��A�A� �A�VA� �A�ffA���A�ZA���A{|�AwAtM�Am�TAgO�AeXAbQ�A_�A[��AW7LAT�HAS�
AQS�AMx�AJE�AF��AC��A?"�A=|�A<JA7ƨA6�DA6A�A5�A5&�A2ȴA1�FA/A-ƨA-S�A-%A,1A+�FA*��A)7LA(��A(-A&E�A$  A#p�A"�\A"5?A!�wA!
=A 1'A�A�+A  A��Ap�A;dA�!A��AS�A`BA��A�!AM�A�DA��A�uA�-A�
A?}A�uA9XAr�At�A9XA
�RA
  A
�A
{A	��A	x�A	A�Ax�A��A�A\)A�A�^A/A��A�A�A;dAA �A �uA -@�
=@���@�X@�b@�n�@�@�dZ@�  @��P@�
=@��H@���@�M�@�{@���@��9@��m@���@�A�@�C�@��H@@��T@�&�@��`@��@�w@�X@�  @���@�/@�1'@�@�+@�o@�=q@�1'@�"�@�$�@�-@���@݉7@�`B@�hs@ܬ@��@ڟ�@�$�@١�@���@�j@�j@��;@���@��@��`@�Ĝ@�bN@���@�ff@��T@���@���@�`B@���@Гu@�(�@ϕ�@ϕ�@�l�@�ȴ@�$�@ͺ^@�7L@�Q�@�Z@�r�@�b@ʧ�@��@ɺ^@ɑh@��@�9X@��y@őh@��@�  @Õ�@Å@�;d@�o@�{@��^@��@��@�A�@��w@�;d@���@�V@�-@��^@�V@���@��u@�9X@�;d@�ȴ@�J@��h@�O�@�&�@�&�@��@��@�ƨ@���@���@��-@��@���@�bN@� �@�  @�ƨ@��@�S�@��@��y@�^5@�-@��@��@���@�(�@���@��F@��F@���@�|�@�l�@�S�@���@�E�@�{@��T@���@�/@�Ĝ@��u@�Q�@��@��@�C�@��!@�=q@��@��T@���@�`B@��@��u@�I�@���@�;d@�"�@�o@��y@���@��\@�E�@�@��h@�p�@�/@���@���@�j@�1'@���@��
@���@��P@�\)@���@�@��@���@��^@���@���@��7@�hs@�O�@�V@�9X@��m@�|�@�33@�o@�ȴ@�V@��-@��7@�p�@�?}@��@��`@��@�Z@��@���@�dZ@�\)@�;d@�@��R@���@�v�@�^5@�5?@�@��#@��7@��9@�b@��w@��@�dZ@�S�@�C�@�33@�"�@�@��H@��@��!@�$�@��T@��-@��@��@���@���@�Q�@�(�@��@�1@��m@��@�
=@��H@��H@��@�ff@��@��@�G�@�7L@��/@��@�ƨ@���@�|�@�\)@�;d@���@��R@���@�=q@���@���@��@�p�@�hs@�X@�?}@�7L@�V@���@�hs@�@u�T@jM�@a��@Wl�@Q7L@I��@?K�@8A�@0��@(Ĝ@&5?@�@(�@�@z�@�@�-@"�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AݓuAݓuA݋DA�p�A�O�A�33A�33A��A�
=A��yAܬAܥ�Aܛ�Aܕ�A܏\A܏\A܋DA܇+A܅A܇+A܃A܃A܃A܁A�|�A�|�A�z�A�x�A�t�A�p�A�jA�hsA�^5A�G�A��A�O�A��A�r�A�A�p�A�;dA�-A�bA�Aԛ�A�x�A�ĜA���A�\)A�bA��yA���AѼjAї�A�oA�I�A�{A͋DA�1'A�ĜA�5?A�JA��A�=qA�r�A���AhA�C�A��A�ƨA�^5A�S�A��jA�v�A�A�$�A��;A���A�O�A��A�{A�`BA�ȴA��#A��uA��+A�1'A��^A��A�/A�ƨA��A�(�A���A�A�~�A�+A���A�XA��mA�v�A�&�A�/A�5?A�ZA���A���A���A��A�A� �A�VA� �A�ffA���A�ZA���A{|�AwAtM�Am�TAgO�AeXAbQ�A_�A[��AW7LAT�HAS�
AQS�AMx�AJE�AF��AC��A?"�A=|�A<JA7ƨA6�DA6A�A5�A5&�A2ȴA1�FA/A-ƨA-S�A-%A,1A+�FA*��A)7LA(��A(-A&E�A$  A#p�A"�\A"5?A!�wA!
=A 1'A�A�+A  A��Ap�A;dA�!A��AS�A`BA��A�!AM�A�DA��A�uA�-A�
A?}A�uA9XAr�At�A9XA
�RA
  A
�A
{A	��A	x�A	A�Ax�A��A�A\)A�A�^A/A��A�A�A;dAA �A �uA -@�
=@���@�X@�b@�n�@�@�dZ@�  @��P@�
=@��H@���@�M�@�{@���@��9@��m@���@�A�@�C�@��H@@��T@�&�@��`@��@�w@�X@�  @���@�/@�1'@�@�+@�o@�=q@�1'@�"�@�$�@�-@���@݉7@�`B@�hs@ܬ@��@ڟ�@�$�@١�@���@�j@�j@��;@���@��@��`@�Ĝ@�bN@���@�ff@��T@���@���@�`B@���@Гu@�(�@ϕ�@ϕ�@�l�@�ȴ@�$�@ͺ^@�7L@�Q�@�Z@�r�@�b@ʧ�@��@ɺ^@ɑh@��@�9X@��y@őh@��@�  @Õ�@Å@�;d@�o@�{@��^@��@��@�A�@��w@�;d@���@�V@�-@��^@�V@���@��u@�9X@�;d@�ȴ@�J@��h@�O�@�&�@�&�@��@��@�ƨ@���@���@��-@��@���@�bN@� �@�  @�ƨ@��@�S�@��@��y@�^5@�-@��@��@���@�(�@���@��F@��F@���@�|�@�l�@�S�@���@�E�@�{@��T@���@�/@�Ĝ@��u@�Q�@��@��@�C�@��!@�=q@��@��T@���@�`B@��@��u@�I�@���@�;d@�"�@�o@��y@���@��\@�E�@�@��h@�p�@�/@���@���@�j@�1'@���@��
@���@��P@�\)@���@�@��@���@��^@���@���@��7@�hs@�O�@�V@�9X@��m@�|�@�33@�o@�ȴ@�V@��-@��7@�p�@�?}@��@��`@��@�Z@��@���@�dZ@�\)@�;d@�@��R@���@�v�@�^5@�5?@�@��#@��7@��9@�b@��w@��@�dZ@�S�@�C�@�33@�"�@�@��H@��@��!@�$�@��T@��-@��@��@���@���@�Q�@�(�@��@�1@��m@��@�
=@��H@��H@��@�ff@��@��@�G�@�7L@��/@��@�ƨ@���@�|�@�\)@�;d@���@��R@���@�=q@���@���@��@�p�@�hs@�X@�?}@�7LG�O�@���@�hs@�@u�T@jM�@a��@Wl�@Q7L@I��@?K�@8A�@0��@(Ĝ@&5?@�@(�@�@z�@�@�-@"�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	49B	49B	33B	2-B	2-B	1'B	1'B	0!B	0!B	/B	.B	.B	-B	-B	-B	-B	-B	,B	-B	-B	-B	-B	-B	-B	,B	,B	,B	,B	-B	-B	.B	.B	/B	1'B	=qB	�^B	�B	��B	�B	��B
	7B
	7B
PB
-B
1'B
6FB
cTB
�XB
��B �B,B/B8RB>wBL�BbNB�PBB��B�B�yB  B+B�B,B49BM�BQ�BXBS�BVB[#Be`BXBYB`BB0!B)�B �B�B�B{BPB+BB��B�B��B�B��B�BBĜB�LB�!B��B�bB|�Bm�BcTB\)BVBO�BC�BJB
�B
ĜB
�9B
�!B
��B
��B
�VB
� B
q�B
e`B
J�B
"�B
1B	�TB	ǮB	�B	�B	cTB	VB	H�B	8RB	%�B	{B	
=B	B��B�B�;B��BɺBÖB�}B�dB�}B��B��B��B�}BBBĜBǮBǮBǮBȴBǮBƨBƨBŢBÖBÖBŢBŢBȴBɺB��B��B��B��B�B�B�#B�#B�)B�)B�#B��B��BȴB�;B�ZB�B�B�B�B�mB�yB�yB�B�fB�HB��B��B��B�)B�`B�sB�B�B�B�B�B�B�B�yB�sB�fB�`B�ZB�sB�B�B�B�B�B�B�fB�ZB�fB�ZB�mB��B	B	%B	%B	%B	%B	%B	B	B	%B		7B	
=B	1B	DB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	&�B	+B	(�B	)�B	-B	-B	1'B	2-B	33B	49B	6FB	=qB	A�B	A�B	C�B	F�B	I�B	Q�B	VB	YB	]/B	]/B	aHB	dZB	dZB	bNB	_;B	cTB	l�B	p�B	s�B	t�B	t�B	s�B	w�B	y�B	z�B	|�B	�B	�B	�B	�B	�DB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�3B	�FB	�LB	�RB	�^B	�dB	�jB	�qB	�wB	�}B	��B	��B	��B	��B	�wB	��B	ÖB	ĜB	ĜB	ŢB	ŢB	ŢB	ŢB	ƨB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�#B	�/B	�/B	�5B	�BB	�HB	�HB	�NB	�NB	�NB	�NB	�ZB	�ZB	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
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
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B
	7B
1B
	7B
	7B

=B
DB
DB
DB
JB
VB
hB
hB
hB
hB
hB
bB
bB
bB
bB
bB
bB
�B
�B
�B
%�B
.B
5?B
:^B
A�B
G�B
L�B
R�B
[#B
_;B
dZB
gmB
l�B
p�B
r�B
w�B
y�B
|�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	4(B	4'B	3!B	2B	2B	1B	1B	0B	0B	/	B	.B	.B	,�B	,�B	,�B	,�B	,�B	+�B	- B	- B	,�B	,�B	,�B	,�B	+�B	+�B	+�B	+�B	,�B	,�B	.B	.B	/B	1B	=_B	�GB	�B	��B	�B	��B
	B
	B
6B
,�B
1B
6*B
c9B
�6B
��B �B+�B.�B80B>SBL�Bb)B�-B�oBͰB��B�SB��BB�B+�B4BM�BQ�BW�BS�BU�BZ�Be9BW�BX�B`B/�B)�B �ByBgBQB$B�B�B��B�B��B�B��B�B�pB�#B��B��B�8B|�BmfBc)B\ BU�BO�BCnB"B
�aB
�rB
�B
��B
��B
�eB
�0B
�B
q�B
e8B
J�B
"�B
B	�/B	ǎB	��B	��B	c3B	U�B	H�B	83B	%�B	\B	
B	 B��B�aB�B��BɜB�xB�^B�EB�^B�kB�kB�fB�^B�pB�nB�~BǌBǊBǊBȕBǏBƇBƅBŁB�uB�vBŀBŁBȓBɘBʡBʡB̫BϽB��B��B�B�B�B�B�B��BͱBȑB�B�9B�nB��B�B�B�HB�UB�SB�YB�AB�$B��B��B��B�B�9B�MB�cB�YB�kB�jB�eB�cB�_B�TB�KB�?B�7B�5B�OB�qB�wB�xB�}B�}B�B�>B�3B�@B�3B�FB��B	�B	�B	�B	�B	�B	�B	�B	�B	�B		B	
B	B	B	AB	dB	vB	rB	wB	sB	iB	qB	iB	oB	qB	vB	�B	&�B	*�B	(�B	)�B	,�B	,�B	0�B	2B	3
B	4B	6B	=EB	A]B	A^B	CiB	F}B	I�B	Q�B	U�B	X�B	\�B	] B	aB	d,B	d-B	b B	_B	c%B	l_B	ptB	s�B	t�B	t�B	s�B	w�B	y�B	z�B	|�B	��B	��B	��B	��B	�B	�GB	�PB	�QB	�jB	�pB	�pB	�qB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�-B	�5B	�9B	�@B	�HB	�KB	�SB	�RB	�RB	�SB	�GB	�RB	�dB	�kB	�mB	�rB	�rB	�pB	�qB	�vB	�}B	�~B	�B	ȂB	ȁB	ȀB	�}B	�}B	ɊB	˔B	̙B	͠B	ΥB	ΦB	ΦB	ΥB	ϭB	дB	ѺB	ѹB	��B	��B	��B	��B	��B	� B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�(B	�(B	�,B	�3B	�<B	�<B	�7B	�;B	�<B	�:B	�AB	�FB	�IB	�RB	�KB	�SB	�SB	�YB	�XB	�]B	�^B	�`B	�`B	�dB	�lB	�jB	�rB	�pB	�pB	�qB	�oB	�pB	�kB	�jB	�rB	�nB	�uB	�}B	�{B	�|B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
 B
	B
	B

B
B
B
B
B
!B
3B
5B
4B
4B
4B
0B
/B
/B
-B
/B
.G�O�B
_B
|B
%�B
-�B
5B
:)B
ARB
GzB
L�B
R�B
Z�B
_B
d!B
g5B
lSB
pnB
rzB
w�B
y�B
|�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.45 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451192016080714511920160807145119  AO  ARCAADJP                                                                    20150701091723    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150701091723  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150701091723  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145119  IP                  G�O�G�O�G�O�                