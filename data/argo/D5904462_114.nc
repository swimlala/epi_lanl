CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-04-05T20:25:42Z AOML 3.0 creation; 2016-08-07T21:51:28Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160405202542  20160807145128  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               rA   AO  5287_9017_114                   2C  D   APEX                            6529                            072314                          846 @עkr���1   @עl`��@.e�S����d�z�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    rA   B   B   @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D!��D"y�D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De�fDf  Df�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtS3DyS3D���D�@ D���D��3D� D�P D�i�D��fD��fD�@ D���Dǳ3D��D�6fD�` D��fD���D�33D�c3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@ə�AffA&ffAFffAfffA�33A�33A�33A�33A�33A�33A�33A�33B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��Bz  B�  B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C ffCffCffCffCffC
ffCffCffC� CffCffCffCffCffCffCffC ffC"ffC$ffC&ffC(ffC*ffC,ffC.ffC0ffC2ffC4ffC6ffC8ffC:ffC<ffC>ffC@ffCBffCDffCF� CHffCJffCLffCNffCPffCRffCTffCVffCXffCZffC\ffC^ffC`ffCbffCdffCfffChffCjffClffCnffCpffCrffCtffCvffCxffCzffC|ffC~ffC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�@ C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�&fC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33D �D ��D�D��D�D��D�D��D�D��D�D��D  D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"4D"�4D#�D#� D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De� Df�Df� Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dtl�Dyl�D�gD�L�D���D�� D��D�\�D�vgD��3D�3D�L�D���D�� D��D�C3D�l�D��3D�	�D�@ D�p D�ɚ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A���A���A���A���A���AΝ�A΅A�S�A�I�A�G�A�A�A�=qA�9XA�7LA�5?A�1'A�-A�1A���A�t�A��A˸RA�
=Aʣ�A��A��
Aɝ�A�K�A���A�VA�ĜAǗ�A�p�A�VA��mA�p�A�+A��TA���A�bNA�p�A�l�A���A���A�t�A���A��7A���A�A�S�A�%A��HA��TA���A�p�A�t�A��A���A�?}A��A�ĜA���A�`BA��A��A�S�A��A�5?A�v�A�ZA���A�p�A��;A�+A��A�r�A�;dA�ffA��!A���A�hsA~-A{`BAz�\AzZAy��Aw�As�PAq�An  Aj��Ae��A`��A`M�A_t�A\�AY�PATbNAR��AQ�mAQ/AP1'AO"�AN1'AL5?AI|�AHZAFbAD�uADI�AC�
AB�HAA/A?��A>�/A>ĜA=�A;�TA:�uA8��A5�;A4�A4E�A4{A3`BA2��A2(�A/A+�A+dZA+\)A*��A*{A)�A(��A'G�A%��A%t�A%K�A%"�A$ȴA$��A$~�A$1'A#l�A"M�A!%A jA��AffA��A1AG�A
=A �A�A�7AM�A{A�hA1'A�;A
�!A	�TA	`BA�A5?A�AƨA7LA��A�AȴAA�A��A �/A �A Z@�|�@�M�@��9@�M�@�Q�@��@��@�A�@�C�@�t�@��F@��@��/@���@�M�@���@�hs@�&�@��
@�ȴ@�5?@�@�/@��@�^5@�V@ꟾ@��@�+@噚@�p�@�hs@�G�@�?}@�/@���@�z�@��;@�
=@�5?@���@��D@�|�@�"�@��@�z�@�l�@�^5@��@��T@���@���@��@��@��T@�`B@�Ĝ@ج@�Z@�l�@�V@ա�@�G�@���@Լj@�A�@�
=@��@�r�@�\)@���@�M�@���@�7L@��`@̓u@��@�;d@���@���@ʧ�@��@�I�@�1'@��@��m@�|�@�o@�~�@�5?@��@ũ�@� �@�K�@�@�E�@���@�A�@��@�l�@���@���@��+@�ff@��@��@�hs@�&�@��@�Ĝ@���@�A�@��@�t�@�l�@�33@�ȴ@�ff@�$�@���@��T@��@�%@�Ĝ@�z�@�b@��@�ƨ@��P@��@��!@�@�?}@���@�Z@�1@�dZ@��@�7L@�bN@��
@���@��@��y@��H@��H@���@��!@�n�@��^@��7@�p�@�X@�/@���@�z�@���@�|�@��@�n�@���@��T@�@���@��@�hs@�G�@�&�@�V@���@���@��@��@��@��/@���@�Z@�9X@��
@��F@��P@�o@�v�@�J@�=q@�^5@�E�@��@���@�l�@�\)@�K�@�C�@�C�@�+@�@��H@��+@�E�@�5?@�J@�&�@��`@���@�1'@�t�@���@�~�@�=q@��@�x�@�?}@��@�V@���@���@��@��@���@��j@��9@��@���@��u@�Z@� �@�  @��@��@��R@�{@��@���@���@�G�@���@���@��y@��R@���@�^5@�M�@�M�@�=q@�J@��@��#@��-@���@��7@�`B@�%@���@��/@��j@��@�Q�@�1@��w@�dZ@�@�n�@���@� �@���@��@��m@��@��@�|�@�dZ@�33@�o@��@���@���@�ff@�$�@�@��@�Z@�  @��@�\)@�C�@�;d@�;d@�
=@��!@�M�@�E�@�5?@�J@��T@�@���@���@���@���@��h@�`B@���@�^5@�V@v�+@k@^��@T1@N@E��@?�@9&�@0�`@+S�@%�@�;@�#@ff@�
@�P@(�@	�#@r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A���A���A���A���A���A���A���A���A���A���A���A���A���AΝ�A΅A�S�A�I�A�G�A�A�A�=qA�9XA�7LA�5?A�1'A�-A�1A���A�t�A��A˸RA�
=Aʣ�A��A��
Aɝ�A�K�A���A�VA�ĜAǗ�A�p�A�VA��mA�p�A�+A��TA���A�bNA�p�A�l�A���A���A�t�A���A��7A���A�A�S�A�%A��HA��TA���A�p�A�t�A��A���A�?}A��A�ĜA���A�`BA��A��A�S�A��A�5?A�v�A�ZA���A�p�A��;A�+A��A�r�A�;dA�ffA��!A���A�hsA~-A{`BAz�\AzZAy��Aw�As�PAq�An  Aj��Ae��A`��A`M�A_t�A\�AY�PATbNAR��AQ�mAQ/AP1'AO"�AN1'AL5?AI|�AHZAFbAD�uADI�AC�
AB�HAA/A?��A>�/A>ĜA=�A;�TA:�uA8��A5�;A4�A4E�A4{A3`BA2��A2(�A/A+�A+dZA+\)A*��A*{A)�A(��A'G�A%��A%t�A%K�A%"�A$ȴA$��A$~�A$1'A#l�A"M�A!%A jA��AffA��A1AG�A
=A �A�A�7AM�A{A�hA1'A�;A
�!A	�TA	`BA�A5?A�AƨA7LA��A�AȴAA�A��A �/A �A Z@�|�@�M�@��9@�M�@�Q�@��@��@�A�@�C�@�t�@��F@��@��/@���@�M�@���@�hs@�&�@��
@�ȴ@�5?@�@�/@��@�^5@�V@ꟾ@��@�+@噚@�p�@�hs@�G�@�?}@�/@���@�z�@��;@�
=@�5?@���@��D@�|�@�"�@��@�z�@�l�@�^5@��@��T@���@���@��@��@��T@�`B@�Ĝ@ج@�Z@�l�@�V@ա�@�G�@���@Լj@�A�@�
=@��@�r�@�\)@���@�M�@���@�7L@��`@̓u@��@�;d@���@���@ʧ�@��@�I�@�1'@��@��m@�|�@�o@�~�@�5?@��@ũ�@� �@�K�@�@�E�@���@�A�@��@�l�@���@���@��+@�ff@��@��@�hs@�&�@��@�Ĝ@���@�A�@��@�t�@�l�@�33@�ȴ@�ff@�$�@���@��T@��@�%@�Ĝ@�z�@�b@��@�ƨ@��P@��@��!@�@�?}@���@�Z@�1@�dZ@��@�7L@�bN@��
@���@��@��y@��H@��H@���@��!@�n�@��^@��7@�p�@�X@�/@���@�z�@���@�|�@��@�n�@���@��T@�@���@��@�hs@�G�@�&�@�V@���@���@��@��@��@��/@���@�Z@�9X@��
@��F@��P@�o@�v�@�J@�=q@�^5@�E�@��@���@�l�@�\)@�K�@�C�@�C�@�+@�@��H@��+@�E�@�5?@�J@�&�@��`@���@�1'@�t�@���@�~�@�=q@��@�x�@�?}@��@�V@���@���@��@��@���@��j@��9@��@���@��u@�Z@� �@�  @��@��@��R@�{@��@���@���@�G�@���@���@��y@��R@���@�^5@�M�@�M�@�=q@�J@��@��#@��-@���@��7@�`B@�%@���@��/@��j@��@�Q�@�1@��w@�dZ@�@�n�@���@� �@���@��@��m@��@��@�|�@�dZ@�33@�o@��@���@���@�ff@�$�@�@��@�Z@�  @��@�\)@�C�@�;d@�;d@�
=@��!@�M�@�E�@�5?@�J@��T@�@���@���@���@���@��h@�`BG�O�@�^5@�V@v�+@k@^��@T1@N@E��@?�@9&�@0�`@+S�@%�@�;@�#@ff@�
@�P@(�@	�#@r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB!�B!�B!�B"�B!�B"�B"�B"�B!�B!�B!�B!�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B(�B.B7LB9XB:^B;dB>wBQ�BhsB�B��BB��B��B�`B49BC�BYBcTB{�B�DB�\B�VB��B��B��B�PB�1B� Bs�B\)BL�B<jB+B�B�B�BPB��B��B�B�B�B�B�fB��B�3B�uBq�BK�B<jB.B  B
��B
ƨB
��B
�qB
�!B
�JB
m�B
]/B
YB
O�B
=qB
<jB
=qB
6FB
)�B
VB	��B	�TB	ŢB	��B	�%B	�1B	�1B	t�B	]/B	=qB	1'B	)�B	#�B	�B	�B	\B	B��B�B�mB�HB�;B�/B�#B�BB�B�
B�NB�`B�/B�
B��BƨBŢBĜBÖBB��B�qB�jB�wB��BĜBƨBȴBɺB��B��B��B�B�
B�
B�B�B�B�B�)B�HB�mB�mB�B�B�fB�;B�5B��B��B��B��BȴBƨBĜB��B�}B�qB�qB�qB�jB�qB�jB�jB�jB�qB�jB�^B��BB��B��B��B�B�
B�B�
B�
B�NB	bB	uB	hB	�B	�B	�B	!�B	.B	6FB	8RB	8RB	8RB	8RB	7LB	5?B	49B	1'B	-B	'�B	$�B	&�B	,B	6FB	<jB	=qB	=qB	=qB	>wB	>wB	?}B	A�B	C�B	G�B	K�B	O�B	P�B	S�B	S�B	T�B	W
B	[#B	^5B	bNB	e`B	ffB	ffB	o�B	r�B	r�B	s�B	t�B	u�B	v�B	w�B	z�B	|�B	~�B	~�B	~�B	~�B	�B	�B	�7B	�VB	�hB	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�3B	�RB	�^B	�jB	�qB	�qB	�qB	�qB	�jB	�qB	�wB	�wB	�wB	�wB	�}B	�}B	�}B	�}B	��B	��B	��B	��B	B	ĜB	ŢB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�)B	�5B	�BB	�NB	�fB	�fB	�fB	�fB	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
%B
%B
%B
+B
+B
+B
+B
+B
%B
%B
%B
%B
%B
%B
%B
%B
1B
	7B
	7B
	7B
DB
PB
PB
PB
PB
JB
JB
DB
PB
PB
PB
VB
VB
VB
VB
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
bB
hB
hB
hB
hB
hB
hB
{B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
�B
$�B
2-B
9XB
?}B
I�B
L�B
P�B
VB
[#B
`BB
e`B
jB
n�B
t�B
v�B
z�B
~�B
�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B!�B!�B!�B"�B!�B"�B"�B"�B!�B!�B!�B!�B!�B�B�BtBqBoBoBoBsBxBuByB}B�B!�B(�B-�B7.B9:B:?B;DB>XBQ�BhVB��B��B�oB̮B��B�?B4BCuBX�Bc0B{�B�#B�<B�6B��B��B�dB�/B�B�Bs�B\BL�B<DB*�B�B�BtB+B��B��B�{B�|B�rB�\B�@BͭB�B�OBq�BK�B<DB-�B
��B
ϸB
ƄB
�[B
�NB
��B
�'B
mnB
]B
X�B
O�B
=LB
<HB
=MB
6$B
)�B
4B	��B	�4B	ŁB	�tB	�B	�B	�B	t�B	]B	=RB	1B	)�B	#�B	�B	fB	?B	B��B�B�RB�0B� B�B�B�(B��B��B�3B�FB�B��BξBƍBŇBĂB�}B�uB�hB�YB�PB�\B�nBĄBƋBȘBɠBʨB̳B��B��B��B��B��B��B��B�B�B�+B�PB�PB�hB�_B�GB�B�B��B��BʤBʣBȘBƈB�~B�fB�`B�TB�SB�QB�KB�SB�KB�MB�JB�TB�MB�CB�eB�oBʣB��B��B��B��B��B��B��B�-B	@B	SB	EB	\B	iB	}B	!�B	-�B	6!B	8/B	8-B	8-B	8/B	7(B	5B	4B	1B	,�B	'�B	$�B	&�B	+�B	6!B	<HB	=KB	=KB	=MB	>PB	>QB	?YB	AdB	CrB	G�B	K�B	O�B	P�B	S�B	S�B	T�B	V�B	Z�B	^B	b%B	e8B	f>B	f>B	oxB	r�B	r�B	s�B	t�B	u�B	v�B	w�B	z�B	|�B	~�B	~�B	~�B	~�B	��B	��B	�B	�,B	�?B	�DB	�EB	�GB	�SB	�YB	�gB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�1B	�?B	�FB	�EB	�HB	�EB	�>B	�FB	�NB	�MB	�NB	�OB	�SB	�RB	�RB	�RB	�^B	�`B	�`B	�_B	�dB	�rB	�vB	�|B	�|B	ȈB	ɐB	˜B	̣B	ϱB	ϱB	ϱB	кB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�8B	�8B	�:B	�8B	�1B	�4B	�2B	�3B	�3B	�7B	�9B	�AB	�?B	�AB	�?B	�BB	�AB	�?B	�HB	�ZB	�^B	�`B	�mB	�wB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
�B
�B
�B
�B
�B
�B
�B
�B
B
		B
	B
	B
B
"B
 B
#B
#B
B
B
B
!B
!B
$B
(B
(B
(B
(B
'B
'B
-B
.B
.B
/B
/B
3B
4B
4B
7B
3B
:B
9B
:B
;B
8B
8B
LB
RB
SB
QB
QB
RB
RB
SB
QB
TB
WB
ZB
VB
XB
YB
XB
WB
WB
fB
kB
kB
pB
qB
sB
sB
qB
vB
wB
xB
uB
|B
|B
B
}B
}B
B
B
~B
{G�O�B
�B
$�B
1�B
9'B
?KB
I�B
L�B
P�B
U�B
Z�B
`B
e1B
jOB
neB
t�B
v�B
z�B
~�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.4 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451282016080714512820160807145128  AO  ARCAADJP                                                                    20160405202542    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160405202542  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160405202542  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145128  IP                  G�O�G�O�G�O�                