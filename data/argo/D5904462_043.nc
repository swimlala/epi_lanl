CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-03-28T09:17:11Z AOML 3.0 creation; 2016-08-07T21:51:16Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150328091711  20160807145116  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               +A   AO  5287_9017_043                   2C  D   APEX                            6529                            072314                          846 @�D��??�1   @�D�l�@1������d�l�C��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    +A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���C  C  C  C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D!��D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy�3D�fD�\�D��3D���D�3D�P D�� D��fD� D�FfD���D��3D��D�Y�Dڃ3D�� D��D�@ D�3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�fg@���AffA&ffAFffAfffA�33A�33A�33A�33A�33A�33A�33A�33B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B�33C L�CffCffCffCL�C
ffCffCffCffCffCffCffCffCffCffCffC ffC"ffC$ffC&ffC(ffC*ffC,ffC.ffC0ffC2ffC4ffC6ffC8ffC:ffC<ffC>ffC@ffCBffCDffCFffCHffCJffCLffCNffCPffCRffCTffCVffCXffCZffC\ffC^ffC`ffCbffCdffCfffChffCjffClffCnffCpffCrffCtffCvffCxffCzffC|ffC~ffC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�@ C�@ C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D� D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"4D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�4Dy��D�3D�i�D�� D��D�  D�\�D���D��3D��D�S3D���D�� D�&gD�fgDڐ D���D��D�L�D� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�7LA�9XA�9XA�7LA�7LA�5?A�+A� �A�$�A�&�A��A�{A�bA��A��;A�ȴA̰!A̬A̧�Ḁ�A̙�A̍PA̅A�x�A�^5A�33A�1'A� �A�bA�A��A���Aˉ7A���A���A�XA˃A�n�A�VA�=qA���A�x�A��A���A�I�A���AȮA�?}A�O�Aƴ9A�"�A�ĜA��A�+A�~�A�hsA���A�I�A�^5A�5?A�7LA�?}A�dZA���A��DA�C�A�$�A��A�ƨA�(�A�JA�\)A��A���A�&�A��7A�ĜA��mA��wA�|�A�/A�&�A�1A�ĜA�C�A�|�A�~�A���A���A���A�p�A�$�A�A���A�I�A�oA��A�n�A�
=A���A�G�A���A��A�5?A�&�A��A�A��A��RA�t�A�ĜA��A�dZA�=qA���A���A|�HAzM�Ax1Ar��Ao�AnA�Ag��Ac�A_��A]�A[l�AZ=qAX1AT�AS��AR�\AP��AP1AO��AN(�AK��AI�7AH�uAE�-AA�A@jA?�A?�A=�7A;�hA9��A89XA6�A3��A2bA0bNA/&�A-��A-dZA,��A+��A+7LA*�A)�TA)hsA(��A't�A&jA%|�A$bA#+A"�9A!�TA �A 1A�Ar�A�A�wAn�A��A?}A �A��A��A��A�mA�A�hA
�jA�HA�RA	��A
n�A	�hAK�A/A	A	33AZA{A1'Az�A��A�A^5A7LA��A1A�A �y@�5?@���@���@��@�p�@�b@��@��H@��y@�\)@�Ĝ@���@�`B@�j@�33@��@�D@�@�n�@��@���@��@�`B@��@�9X@�R@�{@�M�@�5?@�G�@�j@�/@�9X@���@�+@�{@�p�@��/@�9@�Q�@�@�/@��@�Ĝ@���@��T@ܼj@��H@��@�Ĝ@�A�@� �@֟�@�@ղ-@�Ĝ@Ԭ@Ԭ@�z�@��H@�p�@Ѓ@� �@�1@υ@�
=@Ώ\@�E�@��@���@��@���@�O�@� �@��@�Z@��/@�(�@��y@��T@ɩ�@ɩ�@�7L@��/@��@��@���@�1@�\)@�
=@��@��H@��@���@���@Ə\@�~�@���@���@�/@�7L@�/@��@��@��@�%@��/@ļj@�Q�@�(�@��
@Ý�@�dZ@�+@��y@�@�~�@���@�&�@���@��/@��@�A�@�A�@�A�@�A�@�dZ@�@�ȴ@�$�@�x�@���@��@��@�|�@��!@�^5@�=q@��@�O�@��D@���@���@�@�`B@��`@��/@���@�j@�  @��w@���@�dZ@�C�@�o@���@�v�@�^5@�V@�-@�hs@�%@��`@�z�@�(�@��m@��w@�
=@��!@�@�J@��#@��7@�Ĝ@�A�@��F@���@�;d@��H@�{@�x�@��@���@��`@���@��j@�bN@�  @�t�@�33@��@��@�hs@��D@�  @��F@���@�|�@�t�@���@�E�@���@��T@���@���@�G�@��j@�Z@�1'@��m@�t�@���@�^5@��@��@���@�p�@�7L@�&�@��@���@��@���@��u@��@��w@��P@�K�@�@�ȴ@���@�^5@�-@�J@���@���@�?}@���@���@���@��D@��@�j@�A�@�  @��w@�t�@�;d@�o@��@��H@��!@�v�@�{@���@���@���@���@��@�`B@�?}@��@��j@���@�j@�1'@��;@��@�t�@�+@��@��@��y@�ȴ@��\@�^5@��@�p�@��@��u@��m@��P@��y@�A�@�n�@z�!@qx�@i�7@d�D@[C�@SdZ@Ix�@Ax�@:�H@5?}@0r�@*��@$9X@�R@G�@t�@�+@
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�7LA�9XA�9XA�7LA�7LA�5?A�+A� �A�$�A�&�A��A�{A�bA��A��;A�ȴA̰!A̬A̧�Ḁ�A̙�A̍PA̅A�x�A�^5A�33A�1'A� �A�bA�A��A���Aˉ7A���A���A�XA˃A�n�A�VA�=qA���A�x�A��A���A�I�A���AȮA�?}A�O�Aƴ9A�"�A�ĜA��A�+A�~�A�hsA���A�I�A�^5A�5?A�7LA�?}A�dZA���A��DA�C�A�$�A��A�ƨA�(�A�JA�\)A��A���A�&�A��7A�ĜA��mA��wA�|�A�/A�&�A�1A�ĜA�C�A�|�A�~�A���A���A���A�p�A�$�A�A���A�I�A�oA��A�n�A�
=A���A�G�A���A��A�5?A�&�A��A�A��A��RA�t�A�ĜA��A�dZA�=qA���A���A|�HAzM�Ax1Ar��Ao�AnA�Ag��Ac�A_��A]�A[l�AZ=qAX1AT�AS��AR�\AP��AP1AO��AN(�AK��AI�7AH�uAE�-AA�A@jA?�A?�A=�7A;�hA9��A89XA6�A3��A2bA0bNA/&�A-��A-dZA,��A+��A+7LA*�A)�TA)hsA(��A't�A&jA%|�A$bA#+A"�9A!�TA �A 1A�Ar�A�A�wAn�A��A?}A �A��A��A��A�mA�A�hA
�jA�HA�RA	��A
n�A	�hAK�A/A	A	33AZA{A1'Az�A��A�A^5A7LA��A1A�A �y@�5?@���@���@��@�p�@�b@��@��H@��y@�\)@�Ĝ@���@�`B@�j@�33@��@�D@�@�n�@��@���@��@�`B@��@�9X@�R@�{@�M�@�5?@�G�@�j@�/@�9X@���@�+@�{@�p�@��/@�9@�Q�@�@�/@��@�Ĝ@���@��T@ܼj@��H@��@�Ĝ@�A�@� �@֟�@�@ղ-@�Ĝ@Ԭ@Ԭ@�z�@��H@�p�@Ѓ@� �@�1@υ@�
=@Ώ\@�E�@��@���@��@���@�O�@� �@��@�Z@��/@�(�@��y@��T@ɩ�@ɩ�@�7L@��/@��@��@���@�1@�\)@�
=@��@��H@��@���@���@Ə\@�~�@���@���@�/@�7L@�/@��@��@��@�%@��/@ļj@�Q�@�(�@��
@Ý�@�dZ@�+@��y@�@�~�@���@�&�@���@��/@��@�A�@�A�@�A�@�A�@�dZ@�@�ȴ@�$�@�x�@���@��@��@�|�@��!@�^5@�=q@��@�O�@��D@���@���@�@�`B@��`@��/@���@�j@�  @��w@���@�dZ@�C�@�o@���@�v�@�^5@�V@�-@�hs@�%@��`@�z�@�(�@��m@��w@�
=@��!@�@�J@��#@��7@�Ĝ@�A�@��F@���@�;d@��H@�{@�x�@��@���@��`@���@��j@�bN@�  @�t�@�33@��@��@�hs@��D@�  @��F@���@�|�@�t�@���@�E�@���@��T@���@���@�G�@��j@�Z@�1'@��m@�t�@���@�^5@��@��@���@�p�@�7L@�&�@��@���@��@���@��u@��@��w@��P@�K�@�@�ȴ@���@�^5@�-@�J@���@���@�?}@���@���@���@��D@��@�j@�A�@�  @��w@�t�@�;d@�o@��@��H@��!@�v�@�{@���@���@���@���@��@�`B@�?}@��@��j@���@�j@�1'@��;@��@�t�@�+@��@��@��y@�ȴ@��\@�^5@��@�p�@��@��u@��mG�O�@��y@�A�@�n�@z�!@qx�@i�7@d�D@[C�@SdZ@Ix�@Ax�@:�H@5?}@0r�@*��@$9X@�R@G�@t�@�+@
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�)B
�NB
�ZB
�mB
�B
��B
��B
��B
��B
��B
��B
��B
��B%B�BN�By�B�PB�3B��B�BBhB$�B(�B0!B2-B33BE�BK�BI�BF�BG�BG�BG�BI�BJ�BM�BI�BL�BVB^5B\)BR�BS�BT�BS�BI�BH�B_;B_;BcTBk�B`BB_;B_;BR�BW
BW
BW
BW
BW
BW
BT�BL�BE�B>wB5?B,B"�B�B�B+B�mBĜB�-B��B�oBx�BG�B�BhB
=BB
�sB
ɺB
��B
�VB
�7B
�B
v�B
bNB
E�B
7LB
.B
uB	��B	�TB	��B	�?B	��B	��B	p�B	XB	E�B	:^B	7LB	/B	%�B	�B	�B	oB	PB	
=B	+B	B��B�B�yB�ZB�/B�)B�B�B�
B��B��B��B��BɺB��B��B��B��B��B��B��B�B�BB�HB�NB�HB�TB�`B�B�B�B�B�B�B�B��B��B��B��B	B	%B	1B	PB	%B��B�sB�mB�fB�/B��BBŢB�5B�B�B	JB	hB	1B	B	+B		7B	bB	�B	�B	uB	+B	B��B��B��B�B�`B�TB�HB�/B�B�B�B�#B�ZB�B��B	1B	JB	PB	\B	oB	�B	�B	�B	�B	 �B	33B	6FB	8RB	9XB	:^B	=qB	A�B	C�B	E�B	D�B	D�B	C�B	C�B	F�B	J�B	J�B	K�B	K�B	J�B	Q�B	O�B	O�B	P�B	P�B	L�B	J�B	G�B	H�B	I�B	I�B	I�B	M�B	Q�B	T�B	W
B	XB	XB	XB	ZB	\)B	^5B	^5B	^5B	`BB	bNB	e`B	gmB	hsB	jB	m�B	t�B	x�B	z�B	� B	�B	�PB	�\B	�DB	�7B	�7B	�=B	�DB	�PB	�oB	�{B	�{B	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�!B	�!B	�!B	�!B	�'B	�'B	�-B	�3B	�3B	�3B	�?B	�FB	�RB	�RB	�RB	�XB	�dB	�jB	�jB	�qB	�qB	�}B	�}B	�}B	�}B	��B	��B	ĜB	ƨB	ƨB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�#B	�#B	�5B	�;B	�BB	�BB	�NB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�`B	�ZB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
1B
+B
hB
�B
#�B
,B
.B
8RB
:^B
D�B
J�B
Q�B
VB
[#B
_;B
cTB
gmB
k�B
r�B
v�B
z�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
ιB
λB
νB
ιB
λB
ιB
ιB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�.B
�:B
�MB
�hB
��B
��B
��B
��B
��B
��B
��B
��B	B{BN�By�B�.B�B��B�\B �BEB$�B(�B/�B2B3BE�BK�BI�BF�BG�BG�BG�BI�BJ�BM�BI�BL�BU�B^B\BR�BS�BT�BS�BI�BH�B_B_Bc.BkbB` B_B_BR�BV�BV�BV�BV�BV�BV�BT�BL�BE|B>TB5B+�B"�B�BvBB�GB�xB�B��B�IBx�BG�B�BBB
B�B
�QB
ɓB
��B
�4B
�B
��B
v�B
b,B
E~B
7+B
-�B
WB	��B	�4B	��B	� B	��B	�jB	p�B	W�B	E�B	:BB	71B	/B	%�B	�B	lB	WB	5B	
"B	B	 �B��B�B�`B�@B�B�B�B��B��B��B��BοB̲BɞB͸BͻB͹B��B��B��B��B��B�$B�,B�1B�.B�:B�BB�aB�B�xB�tB�{B�B��B��B��B��B��B	�B	B	B	/B	B��B�SB�PB�IB�B̮B�qBŃB�B�B�B	*B	HB	B	�B	
B		B	AB	mB	pB	SB	B	 �B��B��B��B�~B�>B�4B�(B�B��B��B��B�B�;B�wB��B	B	'B	-B	8B	MB	bB	xB	xB	|B	 �B	3B	6"B	8.B	96B	:=B	=NB	AcB	CpB	E�B	DyB	DwB	CpB	CqB	F�B	J�B	J�B	K�B	K�B	J�B	Q�B	O�B	O�B	P�B	P�B	L�B	J�B	G�B	H�B	I�B	I�B	I�B	M�B	Q�B	T�B	V�B	W�B	W�B	W�B	Y�B	\ B	^B	^B	^B	`B	b'B	e8B	gFB	hKB	jWB	mjB	t�B	x�B	z�B	�B	��B	�'B	�3B	�B	�B	�B	�B	�B	�&B	�GB	�RB	�RB	�IB	�LB	�TB	�^B	�dB	�lB	�oB	�rB	�pB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�	B	�	B	�B	�B	�(B	�*B	�)B	�+B	�9B	�AB	�BB	�DB	�GB	�QB	�QB	�QB	�SB	�WB	�^B	�rB	�{B	�|B	ȈB	ȇB	ȈB	ɎB	ʗB	̞B	̡B	ͨB	ͧB	έB	έB	ϲB	ϵB	ϰB	ϳB	иB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	� B	�(B	�/B	�4B	�4B	�6B	�6B	�5B	�5B	�3B	�;B	�9B	�:B	�<B	�4B	�+B	�9B	�LB	�ZB	�aB	�kB	�kB	�sB	�xB	�xB	�wB	�qB	�pB	�rB	�vB	�~B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
;B
wB
#�B
+�B
-�B
8$B
:.B
DlB
J�B
Q�B
U�B
Z�B
_	B
c%B
g?B
kTB
r�B
v�B
z�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.4 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451162016080714511620160807145116  AO  ARCAADJP                                                                    20150328091711    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150328091711  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150328091711  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145116  IP                  G�O�G�O�G�O�                