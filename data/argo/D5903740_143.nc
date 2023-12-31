CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-02-28T20:16:21Z AOML 3.0 creation; 2016-06-01T00:08:29Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160228201621  20160531170829  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_143                   2C  D   APEX                            5374                            041511                          846 @י$h�:1   @י����@:�9Xb�c�x���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�3D�fD�P D��fD��3D��D�I�D�i�D��3D���D�33D���D�ɚD� D�&fDږfD��3D��D�C3D�y�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�ffA33A'33AG33Ag33A���A���A���A���AÙ�Aә�A㙚A�B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC s3Cs3Cs3Cs3Cs3C
s3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3C s3C"s3C$s3C&s3C(s3C*s3C,s3C.s3C0s3C2s3C4s3C6s3C8s3C:s3C<s3C>s3C@s3CBs3CDs3CFs3CHs3CJs3CLs3CNs3CPs3CRs3CTs3CVs3CXs3CZs3C\s3C^s3C`s3Cbs3Cds3Cfs3Chs3Cjs3Cls3Cns3Cps3Crs3Cts3Cvs3Cxs3Czs3C|s3C~s3C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�FgC�FgC�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�,�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�Dy� D��D�^fD���D���D�3D�X D�x D�љD�3D�A�D�� D�� D�fD�4�Dڤ�D��D�3D�Q�D� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��`A��wA�;dA��A��yA�VA��wA���A���A��\A��DA��A�x�A�r�A�n�A�p�A�n�A�jA�jA�^5A�K�A�A�A�;dA�/A�&�A�&�A� �A�1A�%A��yA��wA��A���A�~�A�hsA�K�A�5?A�&�A��A�bA���A��A��A��A��FA��9A���A���A���A��TA���A�;dA���A���A��jA���A�%A�JA�$�A���A�9XA���A��
A�E�A���A�S�A��uA�&�A�n�A��A�K�A��A��A�t�A�M�A�{A��!A��A��^A�v�A���A��7AXA~1A{oAxffAw�AvM�Au�At��At�As�wAsS�As%Ar9XAqp�AqG�AqVAo��Aml�Al�Ak��Aj�`AjȴAj�RAj��AjZAiAh�RAg��Ae�Ae"�Ab��A`��A_��A_l�A_�A^��A^r�A]��A\r�AZ��AY��AXv�AVVAU?}AT�ASAQS�AN��ANA�AM�7AL��AL1AJ�!AH-AG7LAFȴAFM�AFJAE�7ADv�AB  AA+A@�A@�!A@�\A?33A>�A<1A:ffA9t�A81'A5�hA4��A3�;A1�^A0�yA0(�A/�PA/G�A/
=A.�9A.z�A.-A-K�A,��A,-A*(�A)O�A(�A(JA'|�A&�RA%��A%�A$I�A#�A!��A!�A �A ��A �A =qA�A�9AJA��A33A�jAI�A�wA=qA��A�AȴA�;AĜA�A��A��A�PA?}A�`A��AE�A��Ap�A;dAv�A�AoA�;A
=A�\A�Ax�A
=A�A
��A
9XA	��A	�A	/AȴA�#A�+A�A�7AVA��AȴA=qA�yA��A7LA �@���@�x�@��9@��P@���@���@���@���@�O�@�r�@�|�@���@�Q�@�+@��@�|�@�@�`B@�ƨ@��H@��T@�?}@��`@�&�@�-@��@��T@�/@ܴ9@ڏ\@ش9@��@�;d@���@�Z@ҟ�@�9X@��@θR@�5?@�/@���@Ɂ@ȃ@�ȴ@�G�@�ƨ@���@§�@�$�@���@�7L@��/@���@�b@���@�\)@��@��^@��@��;@�S�@��#@���@��@��j@��w@�^5@���@��^@���@���@���@���@�X@��;@���@�M�@��@��#@���@��-@�Ĝ@��w@���@�v�@��\@�{@���@�x�@�O�@�X@�X@��`@�1'@�ff@���@��D@�j@���@�33@���@��@�p�@�X@��@��j@�Q�@��
@��H@�@���@�`B@�V@���@���@��9@���@�I�@��@�ƨ@��P@��@�l�@��H@��+@�ff@���@���@���@��u@��@��@�C�@�v�@�$�@��#@���@�hs@�x�@��#@��9@��@��w@��P@�S�@�o@��H@��R@���@�v�@�ff@�n�@�n�@�ff@�E�@��T@�`B@��`@��@�  @�t�@���@�X@��@�V@�V@�V@�%@��/@�j@�9X@��;@��y@��R@��H@��!@�ff@�5?@�J@���@�p�@�O�@�?}@��@�V@���@���@�Ĝ@��@�r�@�A�@�9X@�1'@�(�@� �@��;@��@���@��+@�ff@�E�@�5?@�$�@��@��@�J@��#@���@��@��@�/@��@��D@�  @��@���@�dZ@�K�@�C�@�@��@���@���@��@�ȴ@���@���@��^@�x�@�%@��/@���@�A�@�1'@��@�  @�;@��@�w@�P@l�@;d@~��@~ȴ@~��@~{@}�T@}p�@|�/@|�j@|j@z-@sS�@k��@b~�@[�
@R~�@JM�@C�@<�D@7�@/;d@)��@$�@!&�@��@��@ƨ@�P@��@  @�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��`A��wA�;dA��A��yA�VA��wA���A���A��\A��DA��A�x�A�r�A�n�A�p�A�n�A�jA�jA�^5A�K�A�A�A�;dA�/A�&�A�&�A� �A�1A�%A��yA��wA��A���A�~�A�hsA�K�A�5?A�&�A��A�bA���A��A��A��A��FA��9A���A���A���A��TA���A�;dA���A���A��jA���A�%A�JA�$�A���A�9XA���A��
A�E�A���A�S�A��uA�&�A�n�A��A�K�A��A��A�t�A�M�A�{A��!A��A��^A�v�A���A��7AXA~1A{oAxffAw�AvM�Au�At��At�As�wAsS�As%Ar9XAqp�AqG�AqVAo��Aml�Al�Ak��Aj�`AjȴAj�RAj��AjZAiAh�RAg��Ae�Ae"�Ab��A`��A_��A_l�A_�A^��A^r�A]��A\r�AZ��AY��AXv�AVVAU?}AT�ASAQS�AN��ANA�AM�7AL��AL1AJ�!AH-AG7LAFȴAFM�AFJAE�7ADv�AB  AA+A@�A@�!A@�\A?33A>�A<1A:ffA9t�A81'A5�hA4��A3�;A1�^A0�yA0(�A/�PA/G�A/
=A.�9A.z�A.-A-K�A,��A,-A*(�A)O�A(�A(JA'|�A&�RA%��A%�A$I�A#�A!��A!�A �A ��A �A =qA�A�9AJA��A33A�jAI�A�wA=qA��A�AȴA�;AĜA�A��A��A�PA?}A�`A��AE�A��Ap�A;dAv�A�AoA�;A
=A�\A�Ax�A
=A�A
��A
9XA	��A	�A	/AȴA�#A�+A�A�7AVA��AȴA=qA�yA��A7LA �@���@�x�@��9@��P@���@���@���@���@�O�@�r�@�|�@���@�Q�@�+@��@�|�@�@�`B@�ƨ@��H@��T@�?}@��`@�&�@�-@��@��T@�/@ܴ9@ڏ\@ش9@��@�;d@���@�Z@ҟ�@�9X@��@θR@�5?@�/@���@Ɂ@ȃ@�ȴ@�G�@�ƨ@���@§�@�$�@���@�7L@��/@���@�b@���@�\)@��@��^@��@��;@�S�@��#@���@��@��j@��w@�^5@���@��^@���@���@���@���@�X@��;@���@�M�@��@��#@���@��-@�Ĝ@��w@���@�v�@��\@�{@���@�x�@�O�@�X@�X@��`@�1'@�ff@���@��D@�j@���@�33@���@��@�p�@�X@��@��j@�Q�@��
@��H@�@���@�`B@�V@���@���@��9@���@�I�@��@�ƨ@��P@��@�l�@��H@��+@�ff@���@���@���@��u@��@��@�C�@�v�@�$�@��#@���@�hs@�x�@��#@��9@��@��w@��P@�S�@�o@��H@��R@���@�v�@�ff@�n�@�n�@�ff@�E�@��T@�`B@��`@��@�  @�t�@���@�X@��@�V@�V@�V@�%@��/@�j@�9X@��;@��y@��R@��H@��!@�ff@�5?@�J@���@�p�@�O�@�?}@��@�V@���@���@�Ĝ@��@�r�@�A�@�9X@�1'@�(�@� �@��;@��@���@��+@�ff@�E�@�5?@�$�@��@��@�J@��#@���@��@��@�/@��@��D@�  @��@���@�dZ@�K�@�C�@�@��@���@���@��@�ȴ@���@���@��^@�x�@�%@��/@���@�A�@�1'@��@�  @�;@��@�w@�P@l�@;d@~��@~ȴ@~��@~{@}�T@}p�@|�/@|�j@|j@z-@sS�@k��@b~�@[�
@R~�@JM�@C�@<�D@7�@/;d@)��@$�@!&�@��@��@ƨ@�P@��@  @�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB:^B;dB=qB@�BB�BD�BE�BE�BE�BE�BE�BF�BF�BF�BF�BF�BF�BG�BF�BF�BD�BC�BC�BC�BC�BC�BB�BA�BA�B@�B?}BA�B?}B=qB<jB:^B9XB8RB7LB8RB9XB;dB<jB:^B5?B�B%B�B�#B�XB��Bs�BD�B&�BVB�
B�jB��B�B{�Bw�Bt�Br�BhsBI�B-B!�B�B	7B
��B
�B
�B
�fB
�BB
�/B
�B
��B
��B
ƨB
B
�LB
��B
��B
�PB
v�B
e`B
\)B
VB
Q�B
M�B
I�B
H�B
G�B
D�B
>wB
9XB
8RB
6FB
+B
�B
�B
DB
B
B
B
B
B	��B	�B	�sB	�B	��B	�qB	�B	��B	��B	��B	��B	��B	��B	�PB	�B	{�B	s�B	iyB	e`B	bNB	ZB	K�B	@�B	<jB	9XB	6FB	0!B	(�B	�B	�B	�B	�B	{B	hB	DB	B	B	  B��B��B��B��B�mB�)B�
B��BƨBĜB��B�jB�LB�3B�'B�!B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B�uB�hB�bB�\B�\B�VB�JB�=B�+B�B�B�B� B}�Bz�Bv�Bt�Bq�Bo�Bm�BjBgmBffBffBe`BdZBbNBaHB_;B^5B\)B[#BXBVBR�BO�BM�BM�BK�BJ�BH�BI�BH�BH�BG�BF�BE�BC�BA�B@�B?}B?}B>wB=qB<jB9XB8RB5?B5?B33B33B33B2-B2-B1'B1'B0!B0!B/B.B.B-B,B,B+B+B)�B'�B'�B(�B(�B'�B%�B#�B$�B$�B&�B'�B&�B,B.B.B.B,B0!B0!B2-B1'B0!B/B-B+B,B,B,B.B1'B33B33B49B49B49B49B49B49B5?B5?B8RB;dB9XB:^B?}B@�B@�BA�BB�BD�BG�BG�BF�BF�BG�BG�BJ�BM�BQ�BVBYBZB[#BZBZB\)B_;BcTBk�Bo�Bt�Bu�Bv�Bv�Bx�Bx�Bx�Bv�Bu�Bw�Bx�Bx�By�B{�B|�B�B�B�B�B�%B�+B�7B�PB�hB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�3B�?B�FB�RB�^B�qBBĜBŢBƨBǮB��B��B��B��B��B�
B�
B�B�B�B�)B�NB�ZB�mB�B�B�B��B��B��B��B��B��B��B	  B	  B	  B	%B	1B	%B	1B	
=B	DB	JB	PB	hB	uB	uB	�B	�B	�B	�B	�B	�B	"�B	$�B	%�B	%�B	%�B	&�B	&�B	.B	1'B	49B	5?B	6FB	6FB	7LB	8RB	:^B	=qB	>wB	@�B	A�B	B�B	I�B	N�B	S�B	W
B	XB	XB	ZB	\)B	]/B	]/B	^5B	`BB	cTB	iyB	jB	jB	jB	l�B	o�B	q�B	r�B	t�B	w�B	x�B	y�B	{�B	|�B	}�B	~�B	~�B	� B	�B	�B	�B	�B	�%B	�+B	�1B	�DB	�DB	�hB	��B	�LB	��B	�B	��B
\B
�B
#�B
.B
49B
=qB
D�B
K�B
P�B
VB
[#B
aHB
ffB
k�B
p�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B:8B;?B=KB@^BBiBDwBE|BE|BE|BE}BE}BF�BF�BF�BF�BF�BF�BG�BF�BF�BDwBCsBCqBCrBCpBCnBBjBAfBAcB@[B?WBAdB?SB=HB<CB:9B93B8+B7(B8*B90B;AB<FB:7B5B�B�B�B��B�.B��Bs�BDoB&�B,B��B�@B��B��B{�Bw�Bt�Br�BhHBI�B,�B!�BeB	B
��B
�B
�{B
�:B
�B
�B
��B
��B
˞B
ƀB
�dB
� B
��B
�jB
�'B
v�B
e7B
[�B
U�B
Q�B
M�B
I�B
H�B
G�B
DtB
>NB
9/B
8)B
6B
*�B
�B
YB
B
�B
�B
�B
�B
 �B	��B	�B	�KB	��B	��B	�JB	��B	��B	��B	��B	��B	��B	�iB	�,B	��B	{�B	s�B	iUB	e=B	b)B	Y�B	K�B	@cB	<FB	94B	6$B	0 B	(�B	�B	B	pB	eB	ZB	HB	 B	�B	 �B��B��B��B��B��B�KB�	B��BϾBƆBĀB�jB�KB�.B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�jB�VB�JB�CB�=B�=B�5B�+B� B�B��B��B��B�B}�Bz�Bv�Bt�Bq�Bo�BmsBjbBgQBfJBfIBeBBd;Bb0Ba+B_B^B\B[BW�BU�BR�BO�BM�BM�BK�BJ�BH�BI�BH�BH�BG�BF�BE�BCxBAkB@eB?_B?^B>YB=VB<NB9=B85B5 B5!B3B3B3B2B1�B1B1B0B0B.�B-�B-�B,�B+�B+�B*�B*�B)�B'�B'�B(�B(�B'�B%�B#�B$�B$�B&�B'�B&�B+�B-�B-�B-�B+�B0B0B2B1B0B.�B,�B*�B+�B+�B+�B-�B1B3B3B4B4B4B4B4B4B5B5B82B;DB98B:<B?]B@_B@aBAgBBnBDyBG�BG�BF�BF�BG�BG�BJ�BM�BQ�BU�BX�BY�B[BY�BY�B\B_Bc/Bk_Bo|Bt�Bu�Bv�Bv�Bx�Bx�Bx�Bv�Bu�Bw�Bx�Bx�By�B{�B|�B��B��B��B��B� B�B�B�*B�CB�PB�TB�_B�^B�lB�rB�~B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�)B�5B�IB�eB�rB�uB�~BǁBʘBϳBѾB��B��B��B��B��B��B��B� B�&B�/B�AB�VB�dB�lB��B��B��B��B��B��B��B��B��B��B	�B	B	�B	B	
B	B	B	$B	<B	GB	GB	TB	XB	YB	mB	}B	�B	"�B	$�B	%�B	%�B	%�B	&�B	&�B	-�B	0�B	4B	5B	6B	6B	7B	8"B	:0B	=AB	>EB	@VB	AYB	B`B	I�B	N�B	S�B	V�B	W�B	W�B	Y�B	[�B	\�B	\�B	^B	`B	c$B	iJB	jMB	jOB	jOB	lZB	omB	qxB	r~B	t�B	w�B	x�B	y�B	{�B	|�B	}�B	~�B	~�B	�B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�6B	�XB	�B	��B	�SB	��B
&B
iB
#�B
-�B
4B
=;B
DeB
K�B
P�B
U�B
Z�B
aB
f/B
kMB
plB
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.45 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708292016053117082920160531170829  AO  ARCAADJP                                                                    20160228201621    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160228201621  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160228201621  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170829  IP                  G�O�G�O�G�O�                