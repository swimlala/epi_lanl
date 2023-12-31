CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-06-20T19:16:55Z AOML 3.0 creation; 2016-08-07T21:51:18Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150620191655  20160807145118  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ;A   AO  5287_9017_059                   2C  D   APEX                            6529                            072314                          846 @�Y�j1@1   @�Y� @0��"���d�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ;A   B   B   @�33@�  A   A   AC33A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBpffBx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D���D�I�D�� D�� D��D�L�D�vfD��3D�fD�FfD�p D���D� D�@ Dڜ�D�ɚD�	�D�9�D�i�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�ffA33A'33AJffAg33A���A���A���A���AÙ�Aә�A㙚A�B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bj33Br33By��B��fB��fB��fB��fB��fB��fB��fB��B��fB��fB��fB��fB��fB��fB��3B��fB��fB��B��fB̳3Bг3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC s3Cs3Cs3Cs3Cs3C
s3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3C s3C"s3C$s3C&s3C(��C*s3C,s3C.s3C0s3C2s3C4s3C6s3C8s3C:s3C<s3C>s3C@s3CBs3CDs3CFs3CHs3CJs3CLs3CNs3CPs3CRs3CTs3CVs3CXs3CZs3C\s3C^s3C`s3Cbs3Cds3Cfs3Chs3Cjs3Cls3Cns3Cps3Crs3Cts3Cvs3Cxs3Czs3C|s3C~s3C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�,�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�FgC�FgC�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�3Dy�gD�3D�X D��fD��fD�( D�[3D���D�љD�$�D�T�D�~fD��3D�fD�NfDګ3D�� D� D�H D�x D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A��yA���A�ȴAڬA�dZA�A�ĜAٕ�A�p�A�E�A��A��TA�A�ZA�l�Aպ^A�{Aԛ�A�"�AѰ!A�JA�
=A�bA���A�=qA�ȴA�ffA� �AǅA���A�;dA��
A��A�?}A���A��;A��A��-A��A��A�5?A���A���A���A���A��A�7LA�I�A�{A�hsA���A�r�A�/A���A�ffA�A��7A�%A��\A��A���A�A�$�A�1A�A�A�ĜA��^A�"�A���A�x�A��yA��wA�z�A�{A��;A�7LA��uA���A�G�A�7AzE�Aq7LAm��AjM�AgO�Ad  A`{A]K�A[�FAZVAX�HAW&�AUp�ASXAPjAN�AI�
AFr�AB��A?+A=�FA<��A:n�A8-A65?A4��A3hsA1�A0�A/��A-�A+�A*�A)��A&�/A%7LA#�hA!G�Ar�A��Az�A�AA��Av�A��AXA&�AVA�A��A��AC�A��A?}A��A��A�AQ�A��A"�AoA�A�`A�/AffAG�A
�jA
�+A
{A�A�-Ax�Ap�Al�A`BAC�A�HAn�A��A��AĜAȴA�uA^5AbA��A�A �A�mA��A%@��@�$�@���@��#@��T@���@�Ĝ@��
@���@��@�"�@�hs@���@� �@��@�&�@��`@��D@� �@��@�?}@��@��@�`B@�/@��/@�j@�r�@�|�@�M�@��@��@��@�v�@�%@�Q�@�1@ۥ�@�\)@�;d@�
=@�$�@�7L@ش9@��@׍P@�"�@�+@�|�@���@�ƨ@ו�@�|�@�l�@�;d@ָR@�5?@���@�@�x�@�/@Ԭ@�bN@�(�@��m@�S�@�
=@���@�@љ�@�`B@�&�@���@Ь@�A�@��@ϥ�@�+@��@�7L@�V@��`@���@̣�@�^5@�%@�t�@�V@ź^@��/@öF@�\)@�33@�@���@§�@�E�@���@��@���@���@�t�@�|�@�|�@�
=@�v�@�5?@���@�x�@��9@�  @�ƨ@��w@��w@���@��@��\@���@�%@���@��j@��D@�j@�I�@��
@�;d@���@�J@���@�hs@�Ĝ@�I�@� �@��P@�S�@�C�@�"�@��H@���@�~�@��@�G�@��@�%@��`@��@�z�@�j@�A�@���@�l�@�+@��+@�5?@�@��@��T@��T@��-@�p�@�G�@�G�@�7L@��`@��D@�j@�(�@���@��;@���@�dZ@�"�@��R@�V@�{@�@���@��T@���@�`B@�G�@�7L@��@��9@���@��D@��@�Z@��@��m@��w@��F@���@�|�@�C�@�o@�
=@���@�n�@�$�@���@��T@��^@�x�@�?}@��@��/@��D@�I�@��@��@�|�@��H@���@�ff@�-@�@���@��^@�hs@�G�@�O�@�/@��j@�(�@��
@���@�l�@�S�@�33@���@�^5@��@���@�O�@�7L@�%@��D@�1'@��@��P@�l�@�S�@�33@�
=@��\@�M�@�-@�J@��^@��7@�p�@�G�@��@�Ĝ@��D@�z�@�Q�@�1'@�b@���@��w@��@�dZ@�;d@���@�v�@�5?@���@�hs@�O�@��@��`@��@�(�@�ƨ@�C�@��@���@���@�M�@��@�x�@��`@�I�@��;@���@�l�@��@��H@���@�n�@�E�@��-@�`B@�X@�%@���@�9X@���@��@�;@vff@l�D@aX@Z�@P�u@Ihs@?�;@;��@3@,I�@%�@ ��@��@ȴ@�@�T@
�\@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A��yA���A�ȴAڬA�dZA�A�ĜAٕ�A�p�A�E�A��A��TA�A�ZA�l�Aպ^A�{Aԛ�A�"�AѰ!A�JA�
=A�bA���A�=qA�ȴA�ffA� �AǅA���A�;dA��
A��A�?}A���A��;A��A��-A��A��A�5?A���A���A���A���A��A�7LA�I�A�{A�hsA���A�r�A�/A���A�ffA�A��7A�%A��\A��A���A�A�$�A�1A�A�A�ĜA��^A�"�A���A�x�A��yA��wA�z�A�{A��;A�7LA��uA���A�G�A�7AzE�Aq7LAm��AjM�AgO�Ad  A`{A]K�A[�FAZVAX�HAW&�AUp�ASXAPjAN�AI�
AFr�AB��A?+A=�FA<��A:n�A8-A65?A4��A3hsA1�A0�A/��A-�A+�A*�A)��A&�/A%7LA#�hA!G�Ar�A��Az�A�AA��Av�A��AXA&�AVA�A��A��AC�A��A?}A��A��A�AQ�A��A"�AoA�A�`A�/AffAG�A
�jA
�+A
{A�A�-Ax�Ap�Al�A`BAC�A�HAn�A��A��AĜAȴA�uA^5AbA��A�A �A�mA��A%@��@�$�@���@��#@��T@���@�Ĝ@��
@���@��@�"�@�hs@���@� �@��@�&�@��`@��D@� �@��@�?}@��@��@�`B@�/@��/@�j@�r�@�|�@�M�@��@��@��@�v�@�%@�Q�@�1@ۥ�@�\)@�;d@�
=@�$�@�7L@ش9@��@׍P@�"�@�+@�|�@���@�ƨ@ו�@�|�@�l�@�;d@ָR@�5?@���@�@�x�@�/@Ԭ@�bN@�(�@��m@�S�@�
=@���@�@љ�@�`B@�&�@���@Ь@�A�@��@ϥ�@�+@��@�7L@�V@��`@���@̣�@�^5@�%@�t�@�V@ź^@��/@öF@�\)@�33@�@���@§�@�E�@���@��@���@���@�t�@�|�@�|�@�
=@�v�@�5?@���@�x�@��9@�  @�ƨ@��w@��w@���@��@��\@���@�%@���@��j@��D@�j@�I�@��
@�;d@���@�J@���@�hs@�Ĝ@�I�@� �@��P@�S�@�C�@�"�@��H@���@�~�@��@�G�@��@�%@��`@��@�z�@�j@�A�@���@�l�@�+@��+@�5?@�@��@��T@��T@��-@�p�@�G�@�G�@�7L@��`@��D@�j@�(�@���@��;@���@�dZ@�"�@��R@�V@�{@�@���@��T@���@�`B@�G�@�7L@��@��9@���@��D@��@�Z@��@��m@��w@��F@���@�|�@�C�@�o@�
=@���@�n�@�$�@���@��T@��^@�x�@�?}@��@��/@��D@�I�@��@��@�|�@��H@���@�ff@�-@�@���@��^@�hs@�G�@�O�@�/@��j@�(�@��
@���@�l�@�S�@�33@���@�^5@��@���@�O�@�7L@�%@��D@�1'@��@��P@�l�@�S�@�33@�
=@��\@�M�@�-@�J@��^@��7@�p�@�G�@��@�Ĝ@��D@�z�@�Q�@�1'@�b@���@��w@��@�dZ@�;d@���@�v�@�5?@���@�hs@�O�@��@��`@��@�(�@�ƨ@�C�@��@���@���@�M�@��@�x�@��`@�I�@��;@���@�l�@��@��H@���@�n�@�E�@��-@�`B@�X@�%@���G�O�@���@��@�;@vff@l�D@aX@Z�@P�u@Ihs@?�;@;��@3@,I�@%�@ ��@��@ȴ@�@�T@
�\@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
{B
{B
�B
�B
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
)�B
49B
:^B
H�B
aHB
� B
�+B
�1B
�1B
�1B
�+B
�%B
�B
�JB
��B
�LB
�-B
�9B
�B
��B�B2-B@�BR�B_;Be`B~�B��B�B�9B�XB��BBƨB�BB��BBJBDBBB�B�B+B�B�B��B��B��B�5B�B�`B�;B�)B�#B�mB��B�9B��B�oB� BjBR�B6FB33B&�B	7B
�
B
B
�?B
��B
�B
x�B
r�B
hsB
YB
G�B
5?B
'�B
1B	�TB	��B	�+B	r�B	dZB	XB	N�B	G�B	>wB	8RB	2-B	)�B	 �B	�B	JB	B��B�B�mB�TB�HB�;B�)B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�)B�TB�mB�mB�sB�sB�mB�mB�yB�mB�`B�`B�fB�fB�fB�`B�`B�sB�sB�B�B�yB�B�B�B�B�sB�B�B�B�B�B�B�B�B�B��B��B	B	B	B	B	B	B	B	%B	%B	B	B	B	B	B	B	B	B	B	B	B��B��B��B��B��B	  B	B	B	B	+B	
=B	uB	�B	\B	�B	#�B	)�B	.B	/B	1'B	0!B	.B	/B	0!B	0!B	33B	49B	49B	6FB	7LB	7LB	6FB	8RB	<jB	@�B	E�B	H�B	L�B	O�B	XB	\)B	aHB	e`B	ffB	ffB	gmB	iyB	k�B	k�B	l�B	p�B	q�B	s�B	t�B	v�B	v�B	w�B	w�B	w�B	x�B	z�B	{�B	|�B	|�B	|�B	|�B	|�B	|�B	�B	�B	�B	�B	�%B	�B	�B	�B	�B	~�B	}�B	}�B	{�B	|�B	}�B	~�B	~�B	~�B	~�B	~�B	�B	�B	�B	�B	�1B	�PB	�VB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�9B	�9B	�?B	�LB	�RB	�^B	�dB	�dB	�dB	�jB	�jB	�qB	�qB	�wB	�}B	�}B	�}B	�}B	��B	��B	��B	��B	B	B	ÖB	ÖB	ĜB	ŢB	ƨB	ƨB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�)B	�5B	�5B	�5B	�5B	�5B	�;B	�BB	�BB	�BB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�`B	�fB	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
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
B
B
B
B
B
%B
%B
%B
+B
1B
1B
	7B
	7B
	7B

=B

=B

=B
PB
VB
VB
PB
VB
bB
oB
uB
uB
�B
�B
�B
%�B
.B
49B
;dB
C�B
H�B
L�B
S�B
XB
^5B
cTB
gmB
k�B
r�B
t�B
y�B
}�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
gB
aB
_B
fB
gB
`B
ZB
bB
iB
gB
gB
kB
yB
yB
�B
�B
�B
#�B
)�B
4B
:AB
H�B
a*B
�B
�B
�B
�B
�B
�B
�B
�B
�.B
�sB
�,B
�B
�B
��B
��BvB2
B@_BR�B_Be;B~�B��B��B�B�4B�_B�jBƀB�B��B�B"BB�B �B`BhB B�[B��B��B̧B̥B�B�iB�;B�B��B��B�DB��B�B��B�CB�BjWBR�B6B3B&�B	B
��B
�eB
�B
��B
��B
x�B
r�B
hJB
X�B
G�B
5B
'�B

B	�1B	��B	�B	r�B	d9B	W�B	N�B	G�B	>TB	80B	2B	)�B	 �B	lB	(B	�B��B�vB�MB�6B�'B�B�	B� B��B��B��B��B��B��B��B��B��BϾB��B��B��B��B��B��B��B��B��B�B�/B�HB�IB�MB�NB�GB�GB�VB�KB�:B�;B�AB�BB�?B�;B�:B�OB�OB�ZB�XB�TB�_B�fB�_B�ZB�NB�eB�sB�xB�xB�|B�B��B�B�B��B��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	�B	�B	�B	�B	�B	�B	�B	 �B��B��B��B��B��B��B	�B	�B	�B	B	
B	JB	]B	4B	�B	#�B	)�B	-�B	.�B	0�B	/�B	-�B	.�B	/�B	/�B	3B	4B	4B	6B	7 B	7B	6B	8&B	<>B	@WB	EwB	H�B	L�B	O�B	W�B	[�B	aB	e4B	f8B	f8B	g>B	iLB	kWB	kXB	l\B	pvB	q{B	s�B	t�B	v�B	v�B	w�B	w�B	w�B	x�B	z�B	{�B	|�B	|�B	|�B	|�B	|�B	|�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	~�B	}�B	}�B	{�B	|�B	}�B	~�B	~�B	~�B	~�B	~�B	��B	��B	��B	��B	�B	�B	�'B	�4B	�DB	�LB	�VB	�WB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	� B	�,B	�3B	�4B	�3B	�:B	�9B	�>B	�@B	�EB	�JB	�KB	�KB	�KB	�PB	�SB	�WB	�XB	�]B	�^B	�bB	�cB	�gB	�nB	�vB	�vB	�vB	�B	ȂB	ɊB	ɉB	ʑB	˓B	˕B	̜B	̛B	͠B	͡B	͢B	ΨB	ϮB	ѹB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�	B	�B	�B	�B	�B	�B	� B	�!B	�!B	�!B	�!B	�'B	�)B	�-B	�4B	�1B	�3B	�:B	�GB	�LB	�KB	�SB	�TB	�WB	�YB	�gB	�fB	�dB	�cB	�eB	�jB	�lB	�kB	�pB	�qB	�qB	�nB	�{B	�{B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	B
	B

B

B


B
B
B
#B
B
 B
.B
9B
CB
AG�O�B
ZB
�B
%�B
-�B
4B
;/B
C]B
H�B
L�B
S�B
W�B
]�B
cB
g5B
kLB
ryB
t�B
y�B
}�B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.45 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451182016080714511820160807145118  AO  ARCAADJP                                                                    20150620191655    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150620191655  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150620191655  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145118  IP                  G�O�G�O�G�O�                