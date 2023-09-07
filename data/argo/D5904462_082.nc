CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-20T02:15:44Z AOML 3.0 creation; 2016-08-07T21:51:22Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20151020021544  20160807145122  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               RA   AO  5287_9017_082                   2C  D   APEX                            6529                            072314                          846 @�x7q�7�1   @�x8`�@0�I�^�d���$�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    RA   B   B   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB��B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DLy�DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDt` Dy` D��D�S3D�� D��3D�fD�@ D�y�D��3D�	�D�FfD�l�Dǰ D�fD�C3Dڃ3D��3D�3D�,�D�vfD��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @Y��@���@���AffA&ffAFffAfffA�33A�33A�33A�33A�33A�33A�33A�33B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��Bz  B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C ffC� CffCffCffC
ffCffCffCffCffCffCffCffCffCffC� C ffC"ffC$ffC&ffC(ffC*ffC,ffC.ffC0ffC2ffC4ffC6ffC8ffC:ffC<ffC>ffC@ffCBffCDffCFffCHffCJffCLffCN� CPffCRffCTffCVffCXffCZffC\ffC^ffC`ffCbffCdffCfffChffCjffClffCnffCpffCrffCtffCvffCxffCzffC|ffC~ffC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�@ C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL�4DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt  Dty�Dyy�D�)�D�` D���D�� D�#3D�L�D��gD�� D�gD�S3D�y�DǼ�D�#3D�P Dڐ D�� D� D�9�D�3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�uA�\A�uA�A�x�A�p�A�hsA�XA�XA�XA�S�A�S�A�S�A�Q�A�O�A�O�A�Q�A�Q�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�O�A�K�A�C�A��`A��
A��A��A��A�r�A��A�dZA�Q�A�`BA��TA�
=A�33A͏\A��A̼jA̮Aˡ�A���A�M�A�bNA��/A�;dA���A�A�AƃA�ffA��HAĮA×�A�  A�ĜA�XA��wA���A�1'A��`A���A�VA�+A��HA�{A�M�A���A�A��hA���A�VA��FA���A�
=A�|�A��wA�~�A�hsA�?}A��A�|�A���A���A��mA��\A�p�A�9XA�ƨA�{A��;A�K�A���A�x�A��hA���A���A�oA{oAw��At1Ak�wAc�A`��A`$�A_�A[�;AUƨAU�AR �AM`BAIx�AH5?AF��AD��AB1'A@ZA?ƨA>�A=&�A<��A;�A7�A5��A5��A5`BA4�\A4v�A3�A2�RA0��A.1'A-+A,VA*��A)C�A&�RA%G�A$1'A!�^A!XA ĜA jA {A�A��A�AAAG�AȴAC�A5?A^5A�FAȴAr�AQ�A��A�hA�AZAȴAG�A�+A�A�^A�A�A�A�A\)A��AM�AI�A|�A
�uA	�mAZAx�AoA�RA�!A��A��A~�AE�A5?A�Ax�A�`A�RA�
A�TA�AdZAn�AE�AZA��A��Az�A(�A��A�PA��AA�AVA�TA&�A ĜA �\A �\A �uA V@��
@��!@��@��`@��@��@��@�"�@��#@�V@���@��@�z�@���@�+@�n�@�hs@�@�  @�@�R@�^@�Ĝ@�@���@�h@�G�@�9@ꗍ@�p�@蛦@�Q�@��@�
=@��y@��T@�&�@�r�@�1'@��@��m@�t�@�@��@⟾@�v�@�v�@�ff@�^5@�$�@�@�`B@�I�@��;@߶F@ߍP@�o@�n�@�5?@�{@���@݁@܃@ۅ@���@��@�O�@���@�j@�(�@�  @ם�@�K�@ָR@�ff@�E�@�`B@�z�@�t�@Ѻ^@�r�@�1'@�dZ@��@��y@��@υ@�o@���@Χ�@�$�@�V@˾w@�o@ʧ�@�-@��#@ɲ-@��@��/@ȓu@�r�@Ǖ�@�@�E�@��T@ċD@��
@��@��-@��@���@�ƨ@�dZ@��+@���@�?}@��@���@��@�t�@��@��@�7L@�/@��j@�  @���@��P@�dZ@���@�x�@��/@�z�@��m@��P@��H@�E�@���@���@�p�@�?}@���@��@��D@�9X@�  @��;@��P@��R@��@���@�p�@��@��D@�(�@��m@��w@�
=@���@�V@�5?@��@�{@��@�x�@��`@��9@��u@�1'@��F@�t�@�"�@��@���@��+@�^5@�E�@���@�O�@�?}@���@��/@�Ĝ@���@��@�Z@�1@��m@��w@���@�C�@�
=@��@��y@��@�~�@�5?@���@���@�X@�V@��@���@��@��u@��@�r�@�1'@��;@���@�t�@��@���@��y@�ȴ@�ff@�J@�x�@�`B@�&�@�%@��@���@�(�@���@�
=@��y@��H@���@�E�@�@��7@��@���@�z�@���@�dZ@�K�@�+@�
=@�ȴ@�M�@�-@�{@�@��@���@���@��7@�hs@��@���@���@�Ĝ@���@��D@� �@��@�dZ@�ȴ@�^5@�5?@�{@��#@�p�@�V@���@��u@�X@��u@�V@yX@m�T@e�h@^5?@W�@Nv�@I7L@@Q�@;�
@5��@+S�@$�D@��@��@�@�@p�@
-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�uA�\A�uA�A�x�A�p�A�hsA�XA�XA�XA�S�A�S�A�S�A�Q�A�O�A�O�A�Q�A�Q�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�O�A�K�A�C�A��`A��
A��A��A��A�r�A��A�dZA�Q�A�`BA��TA�
=A�33A͏\A��A̼jA̮Aˡ�A���A�M�A�bNA��/A�;dA���A�A�AƃA�ffA��HAĮA×�A�  A�ĜA�XA��wA���A�1'A��`A���A�VA�+A��HA�{A�M�A���A�A��hA���A�VA��FA���A�
=A�|�A��wA�~�A�hsA�?}A��A�|�A���A���A��mA��\A�p�A�9XA�ƨA�{A��;A�K�A���A�x�A��hA���A���A�oA{oAw��At1Ak�wAc�A`��A`$�A_�A[�;AUƨAU�AR �AM`BAIx�AH5?AF��AD��AB1'A@ZA?ƨA>�A=&�A<��A;�A7�A5��A5��A5`BA4�\A4v�A3�A2�RA0��A.1'A-+A,VA*��A)C�A&�RA%G�A$1'A!�^A!XA ĜA jA {A�A��A�AAAG�AȴAC�A5?A^5A�FAȴAr�AQ�A��A�hA�AZAȴAG�A�+A�A�^A�A�A�A�A\)A��AM�AI�A|�A
�uA	�mAZAx�AoA�RA�!A��A��A~�AE�A5?A�Ax�A�`A�RA�
A�TA�AdZAn�AE�AZA��A��Az�A(�A��A�PA��AA�AVA�TA&�A ĜA �\A �\A �uA V@��
@��!@��@��`@��@��@��@�"�@��#@�V@���@��@�z�@���@�+@�n�@�hs@�@�  @�@�R@�^@�Ĝ@�@���@�h@�G�@�9@ꗍ@�p�@蛦@�Q�@��@�
=@��y@��T@�&�@�r�@�1'@��@��m@�t�@�@��@⟾@�v�@�v�@�ff@�^5@�$�@�@�`B@�I�@��;@߶F@ߍP@�o@�n�@�5?@�{@���@݁@܃@ۅ@���@��@�O�@���@�j@�(�@�  @ם�@�K�@ָR@�ff@�E�@�`B@�z�@�t�@Ѻ^@�r�@�1'@�dZ@��@��y@��@υ@�o@���@Χ�@�$�@�V@˾w@�o@ʧ�@�-@��#@ɲ-@��@��/@ȓu@�r�@Ǖ�@�@�E�@��T@ċD@��
@��@��-@��@���@�ƨ@�dZ@��+@���@�?}@��@���@��@�t�@��@��@�7L@�/@��j@�  @���@��P@�dZ@���@�x�@��/@�z�@��m@��P@��H@�E�@���@���@�p�@�?}@���@��@��D@�9X@�  @��;@��P@��R@��@���@�p�@��@��D@�(�@��m@��w@�
=@���@�V@�5?@��@�{@��@�x�@��`@��9@��u@�1'@��F@�t�@�"�@��@���@��+@�^5@�E�@���@�O�@�?}@���@��/@�Ĝ@���@��@�Z@�1@��m@��w@���@�C�@�
=@��@��y@��@�~�@�5?@���@���@�X@�V@��@���@��@��u@��@�r�@�1'@��;@���@�t�@��@���@��y@�ȴ@�ff@�J@�x�@�`B@�&�@�%@��@���@�(�@���@�
=@��y@��H@���@�E�@�@��7@��@���@�z�@���@�dZ@�K�@�+@�
=@�ȴ@�M�@�-@�{@�@��@���@���@��7@�hs@��@���@���@�Ĝ@���@��D@� �@��@�dZ@�ȴ@�^5@�5?@�{@��#@�p�@�V@���G�O�@�X@��u@�V@yX@m�T@e�h@^5?@W�@Nv�@I7L@@Q�@;�
@5��@+S�@$�D@��@��@�@�@p�@
-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
VB
VB
VB
VB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
W
B
bNB
o�B
�7B
��B
��B
��B
�%B
�B�BC�BZBp�B�B�\B�hB��B  BuB0!B>wBK�BL�BP�BS�BW
B[#B]/BiyBk�BgmBYBD�BP�BXBXBQ�B@�B/B%�B�BB��B�B�)B�B�B��B��B��BȴB��B�}B�-B��B��B�oB� Bn�B`BB@�B�BPB1B
��B
�B
��B
�LB
��B
�+B
^5B
J�B
:^B
	7B	�B	��B	��B	n�B	[#B	VB	N�B	;dB	!�B	�B	VB	  B��B�B�sB�HB�B�B�
B��B�B��B��BǮBĜBĜBÖBB��B�}B�jB�dB�qB�jB�RB�3B�!B��B��B��B��B��B��B�B�B�9B�3B�B��B�B�9B�LB�'B��B��B��B��B�B�B�9B�FB�RB�RB�dB�qB�jB�dB�^B�^B�XB�RB��B��B��B�NB�B�B�mB�TB�HB�HB�HB�TB�TB�ZB�ZB�fB�mB�mB�sB�B��B��B��B	B	uB	-B	N�B	T�B	\)B	e`B	ffB	hsB	l�B	m�B	k�B	ffB	dZB	bNB	^5B	^5B	bNB	ffB	k�B	n�B	q�B	r�B	x�B	y�B	x�B	z�B	{�B	{�B	~�B	� B	�B	�%B	�+B	�%B	�%B	�%B	�=B	�\B	�\B	�hB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�?B	�FB	�RB	�dB	�jB	�qB	�}B	�}B	��B	��B	��B	ÖB	ĜB	ǮB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�
B	�B	�B	��B	��B	��B	��B	ƨB	ÖB	B	��B	��B	��B	ÖB	ǮB	ǮB	ǮB	ǮB	ǮB	ƨB	ĜB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�#B	�#B	�B	�B	�
B	�B	��B	��B	�B	�
B	�
B	�
B	�
B	�
B	�B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�BB	�HB	�HB	�NB	�TB	�TB	�ZB	�fB	�mB	�mB	�sB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
1B
1B
	7B

=B

=B
JB
JB
JB
PB
PB
PB
VB
VB
VB
\B
\B
\B
bB
bB
bB
hB
hB
hB
oB
oB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
#�B
,B
2-B
5?B
=qB
F�B
L�B
R�B
VB
ZB
bNB
hsB
n�B
t�B
w�B
w�B
{�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
U�B
U�B
U�B
U�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
V�B
b5B
o�B
�B
��B
�qB
��B
�	B
��B}BCvBY�Bp�B��B�<B�GBϿB��BUB0 B>[BK�BL�BP�BS�BV�B[B]BiTBkdBgJBX�BD{BP�BW�BW�BQ�B@_B.�B%�BeB�B��B�iB�B��B��B��B��BʙBȏB̩B�XB�
B��B�yB�JB�BnsB`B@]BtB,BB
��B
��B
�fB
�(B
��B
�B
^B
J�B
:;B
	B	�]B	͵B	��B	nzB	[B	U�B	N�B	;GB	!�B	�B	=B��B��B�B�WB�-B��B��B��B��B��B��BξBǖBăBĄB�~B�vB�fB�cB�QB�IB�YB�PB�6B�B�B��B��B��B��B��B��B��B�B�B�B��B��B�B�B�.B�B��B��B��B��B��B��B�B�)B�2B�5B�HB�SB�MB�GB�BB�DB�9B�4B�lB˨B��B�/B�`B�eB�OB�5B�)B�(B�&B�6B�7B�:B�:B�HB�MB�KB�TB�lB��B��B��B	�B	RB	,�B	N�B	T�B	\B	e=B	f@B	hNB	lfB	mlB	kaB	fAB	d4B	b'B	^B	^B	b(B	f?B	kbB	nsB	q�B	r�B	x�B	y�B	x�B	z�B	{�B	{�B	~�B	�B	��B	��B	�B	��B	��B	��B	�B	�3B	�4B	�AB	�?B	�CB	�NB	�mB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�*B	�<B	�@B	�GB	�RB	�SB	�ZB	�ZB	�^B	�nB	�sB	ǄB	ȉB	ȊB	ɐB	ɓB	˝B	ͩB	ΰB	мB	мB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ίB	ʘB	�~B	�kB	�gB	�^B	�ZB	�`B	�kB	ǄB	ǄB	ǅB	ǇB	ǃB	�~B	�oB	�B	ǄB	ǆB	ȊB	ɐB	̢B	̠B	̡B	˛B	˚B	ɏB	ʘB	̢B	˛B	ϵB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	� B	�&B	�)B	�1B	�:B	�BB	�AB	�IB	�AB	�FB	�FB	�QB	�QB	�QB	�`B	�mB	�tB	�pB	�qB	�yB	�wB	�uB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
		B

B

B
B
B
B
"B
"B
!B
+B
(B
)B
/B
-B
.B
1B
6B
5B
:B
:B
;B
@B
AB
>B
EB
GB
HB
KB
SB
QB
YB
ZB
]B
`B
_G�O�B
WB
_B
!�B
#�B
+�B
2 B
5B
=BB
FwB
L�B
R�B
U�B
Y�B
bB
h@B
nhB
t�B
w�B
w�B
{�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.4 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451222016080714512220160807145122  AO  ARCAADJP                                                                    20151020021544    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151020021544  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151020021544  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145122  IP                  G�O�G�O�G�O�                