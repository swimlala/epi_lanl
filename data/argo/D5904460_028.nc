CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:08Z AOML 3.0 creation; 2016-08-07T21:17:33Z UW 3.1 conversion     
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
_FillValue                 �  Ax   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cp   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150226221308  20160807141733  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5285_8895_028                   2C  D   APEX                            6487                            072314                          846 @�4Z牠1   @�4[ww_�@-s�E����d�C��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2�C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D��D�9�D���D��fD��D�P D��fD��3D�	�D�6fD�|�DǙ�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@�\)A�A'�AG�Ag�A��
A��
A��
A��
A��
A��
A��
A��
BQ�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B���B���B���B���B���B���B���B�(�B�B���B���B���B���B���B���B�(�B�B���B���B���B�B�B���B���B���B���B���B���B���B���B���B���C z�C�{Cz�Cz�Cz�C
z�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�C z�C"z�C$z�C&z�C(z�C*z�C,z�C.z�C0�{C2�{C4z�C6z�C8z�C:z�C<z�C>z�C@z�CBz�CDaGCFz�CHz�CJz�CLz�CN�{CPz�CRz�CTz�CVz�CXz�CZz�C\z�C^z�C`z�Cbz�Cdz�Cfz�Chz�Cjz�Clz�Cnz�Cpz�Crz�Ctz�Cvz�Cxz�Czz�C|z�C~z�C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�0�C�=qC�=qC�=qC�0�C�0�C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD%DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�Dy�RD�)D�H�D���D���D�)D�_\D���D�ҏD��D�E�D��)DǨ�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�I�A�Q�A�XA�XA�XA�ZA�ZA�\)A�\)A�\)A�`BA�bNA�dZA�hsA�ffA�hsA�bNA�\)A�;dA��;A���A�A�bNAͧ�A�M�A�JA�Ạ�A�
=A�A˃A˸RA��A��A͗�A�oA˅AʁA�/Aɉ7AȃA�"�A�z�A�oA�%A�`BA�E�A��A��jA��+A��A�l�A�^5A��A�VA��A��RA�/A�I�A�-A���A�-A��\A�?}A���A��HA���A��A���A�A�A���A���A�z�A�\)A���A�oA���A~�Ax�/At{Aq�FAo;dAj=qAgO�Ae+Ab�A^n�AX�uATI�AM�7AK�AJ~�AIAD�HAC��AC�ABA@-A?K�A>Q�A=�wA=
=A<1'A;��A;��A:A�A7�A5�A3�A2VA/p�A.��A.v�A-�^A,��A,~�A+�FA+33A*�A*E�A)��A(  A'�A'"�A&Q�A%A%p�A%�A$5?A#�hA#S�A#7LA"�A"ĜA"�A#%A"(�A"=qA!��A!�;A"�A!��A!hsA ȴA ffAƨA|�AoA��A$�A��AĜAVA�hA�yAv�A�TA�FAXA�A �AhsA�Av�A�FA7LA��A�DA�A��A1'A�^A+A�A��A�;A�`A��A�A
�A
�A
��A
E�A	�TA	�A	hsA	VA�A�\AffA��At�AoA��AA�A  A��AK�A�A�!AbNA��A�hAhsA?}A?}A33A"�AoA�AI�A  A�;A��A��A�A �A �A n�A A�A 1'@���@���@���@�`B@�V@�9X@���@�33@���@�-@���@�`B@��@��u@��@�ȴ@�p�@���@��m@�S�@�
=@�$�@�@�X@�7L@�Ĝ@@�@�5?@�/@��/@�1'@��@���@�C�@�n�@���@�%@��@�V@��@��#@�D@��
@�|�@���@��#@�$�@�J@�h@߶F@�\)@�"�@��@ާ�@ޏ\@��`@۾w@�S�@�o@�n�@�%@�I�@���@׶F@׍P@��@���@�ff@���@Ձ@��`@Դ9@��
@��@�~�@�-@���@�hs@��/@�1'@��;@Ϯ@�|�@�33@�=q@͉7@�/@���@̛�@̓u@̬@̋D@��@�S�@�J@ɺ^@�p�@�/@��/@�b@ǅ@�33@Ɵ�@�{@ź^@�x�@�X@�/@��/@�Ĝ@�I�@þw@å�@�\)@�v�@���@�&�@��9@��w@�ȴ@�@��h@�?}@�Ĝ@�9X@���@�|�@��@���@�$�@���@�O�@���@���@��w@���@�l�@��@���@�=q@���@���@�/@��@�(�@���@�@���@�=q@��@��^@�x�@�O�@�/@�%@���@�(�@�\)@�33@�
=@���@���@���@�n�@�J@��-@���@�G�@���@�Z@��;@�|�@�;d@��@��!@��#@���@�7L@��@���@�r�@�Q�@�(�@��@��m@���@�K�@��@�ȴ@��\@��+@���@���@�~�@�E�@�5?@��@��T@���@�x�@�hs@�X@�/@�V@��9@�bN@�  @���@�+@�o@���@��H@��\@�-@��T@�hs@�V@��/@�Z@� �@��;@�dZ@�@��@���@��\@�V@�-@�{@�@�X@��j@�j@�A�@��@��m@�ƨ@���@�l�@��@���@�ff@�@��@���@��^@��h@�hs@�7L@�/@��@��/@��u@�1'@�ƨ@�l�@�;d@��@���@�=q@�@���@��^@��7@��@�bN@�b@��-@�E�@���@�I�@u�h@j��@a�7@Z-@P��@G;d@?�P@9&�@4z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  A�I�A�Q�A�XA�XA�XA�ZA�ZA�\)A�\)A�\)A�`BA�bNA�dZA�hsA�ffA�hsA�bNA�\)A�;dA��;A���A�A�bNAͧ�A�M�A�JA�Ạ�A�
=A�A˃A˸RA��A��A͗�A�oA˅AʁA�/Aɉ7AȃA�"�A�z�A�oA�%A�`BA�E�A��A��jA��+A��A�l�A�^5A��A�VA��A��RA�/A�I�A�-A���A�-A��\A�?}A���A��HA���A��A���A�A�A���A���A�z�A�\)A���A�oA���A~�Ax�/At{Aq�FAo;dAj=qAgO�Ae+Ab�A^n�AX�uATI�AM�7AK�AJ~�AIAD�HAC��AC�ABA@-A?K�A>Q�A=�wA=
=A<1'A;��A;��A:A�A7�A5�A3�A2VA/p�A.��A.v�A-�^A,��A,~�A+�FA+33A*�A*E�A)��A(  A'�A'"�A&Q�A%A%p�A%�A$5?A#�hA#S�A#7LA"�A"ĜA"�A#%A"(�A"=qA!��A!�;A"�A!��A!hsA ȴA ffAƨA|�AoA��A$�A��AĜAVA�hA�yAv�A�TA�FAXA�A �AhsA�Av�A�FA7LA��A�DA�A��A1'A�^A+A�A��A�;A�`A��A�A
�A
�A
��A
E�A	�TA	�A	hsA	VA�A�\AffA��At�AoA��AA�A  A��AK�A�A�!AbNA��A�hAhsA?}A?}A33A"�AoA�AI�A  A�;A��A��A�A �A �A n�A A�A 1'@���@���@���@�`B@�V@�9X@���@�33@���@�-@���@�`B@��@��u@��@�ȴ@�p�@���@��m@�S�@�
=@�$�@�@�X@�7L@�Ĝ@@�@�5?@�/@��/@�1'@��@���@�C�@�n�@���@�%@��@�V@��@��#@�D@��
@�|�@���@��#@�$�@�J@�h@߶F@�\)@�"�@��@ާ�@ޏ\@��`@۾w@�S�@�o@�n�@�%@�I�@���@׶F@׍P@��@���@�ff@���@Ձ@��`@Դ9@��
@��@�~�@�-@���@�hs@��/@�1'@��;@Ϯ@�|�@�33@�=q@͉7@�/@���@̛�@̓u@̬@̋D@��@�S�@�J@ɺ^@�p�@�/@��/@�b@ǅ@�33@Ɵ�@�{@ź^@�x�@�X@�/@��/@�Ĝ@�I�@þw@å�@�\)@�v�@���@�&�@��9@��w@�ȴ@�@��h@�?}@�Ĝ@�9X@���@�|�@��@���@�$�@���@�O�@���@���@��w@���@�l�@��@���@�=q@���@���@�/@��@�(�@���@�@���@�=q@��@��^@�x�@�O�@�/@�%@���@�(�@�\)@�33@�
=@���@���@���@�n�@�J@��-@���@�G�@���@�Z@��;@�|�@�;d@��@��!@��#@���@�7L@��@���@�r�@�Q�@�(�@��@��m@���@�K�@��@�ȴ@��\@��+@���@���@�~�@�E�@�5?@��@��T@���@�x�@�hs@�X@�/@�V@��9@�bN@�  @���@�+@�o@���@��H@��\@�-@��T@�hs@�V@��/@�Z@� �@��;@�dZ@�@��@���@��\@�V@�-@�{@�@�X@��j@�j@�A�@��@��m@�ƨ@���@�l�@��@���@�ff@�@��@���@��^@��h@�hs@�7L@�/@��@��/@��u@�1'@�ƨ@�l�@�;d@��@���@�=q@�@���@��^@��7@��@�bNG�O�@��-@�E�@���@�I�@u�h@j��@a�7@Z-@P��@G;d@?�P@9&�@4z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;oB	`BB	aHB	bNB	bNB	aHB	bNB	bNB	bNB	bNB	bNB	dZB	dZB	dZB	dZB	dZB	e`B	gmB	k�B	�1B	�;B
�PB\BE�Bs�B~�B�B�Bx�Be`B_;B^5Bs�B�jB�B�yB�sB�;B��BoB"�B=qBN�Be`Bk�BdZB33B8RB`BB��B��B��B��B��B��B� Bn�BB��B�oB`BB �B
�B
��B
ǮB
ÖB
�jB
�dB
��B
ƨB
��B
�B
s�B
T�B
I�B
C�B
$�B
PB	��B	��B	�FB	��B	�bB	u�B	bNB	S�B	D�B	+B	VB��B��B�B�B��B	\B	{B	�B	(�B	?}B	E�B	Q�B	]/B	iyB	q�B	r�B	t�B	� B	�oB	��B	�B	��B	�LB	�-B	�3B	ÖB	��B	�fB
B
\B
�B
�B
�B
!�B
&�B
%�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
'�B
,B
.B
;dB
@�B
B�B
H�B
J�B
G�B
G�B
H�B
I�B
L�B
N�B
N�B
N�B
M�B
C�B
6FB
.B
.B
/B
/B
/B
0!B
1'B
.B
+B
)�B
'�B
&�B
%�B
$�B
&�B
$�B
"�B
"�B
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
!�B
"�B
!�B
!�B
"�B
"�B
"�B
"�B
 �B
"�B
"�B
"�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
hB
\B
\B
\B
VB
VB
VB
PB
PB
JB
DB

=B
	7B
1B
%B
B
%B
%B
%B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
JB
JB
PB
VB
\B
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
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
uB
uB
uB
oB
oB
oB
uB
uB
uB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
'�B
�B
&�B
,B
1'B
8RB
9XB
B�B
K�B
P�B
VB
\)B
aHB
dZ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  B	`)B	a1B	b5B	b8B	a0B	b6B	b6B	b8B	b6B	b8B	dCB	dCB	dCB	dBB	dCB	eFB	gUB	koB	�B	�B
�-B6BE}Bs�B~�B��B��Bx�Be@B_B^Bs�B�CB�}B�SB�MB�B��BFB"�B=IBN�Be6BkZBd2B3
B8'B`B�}B��B��B��B��B�pB�BnnB�B��B�BB`B �B
�gB
ϳB
ǃB
�kB
�>B
�;B
��B
�zB
�qB
��B
s�B
T�B
I�B
CiB
$�B
)B	��B	϶B	�B	��B	�?B	u�B	b(B	S�B	DyB	*�B	2B��B��B�B�B��B	8B	VB	eB	(�B	?VB	EzB	Q�B	]B	iOB	q}B	r�B	t�B	�B	�CB	��B	��B	�]B	�!B	� B	�B	�gB	ϰB	�6B
�B
,B
uB
|B
yB
!�B
&�B
%�B
!�B
{B
nB
nB
]B
OB
UB
\B
yB
"�B
'�B
+�B
-�B
;.B
@PB
B^B
H�B
J�B
G{B
G|B
H�B
I�B
L�B
N�B
N�B
N�B
M�B
CbB
6B
-�B
-�B
.�B
.�B
.�B
/�B
0�B
-�B
*�B
)�B
'�B
&�B
%�B
$�B
&�B
$�B
"�B
"�B
!�B
 �B
 �B
�B
�B
B
wB
B
�B
�B
�B
!�B
"�B
!�B
!�B
"�B
"�B
"�B
"�B
 �B
"�B
"�B
"�B
!�B
 �B
�B
�B
�B
�B
�B
}B
xB
xB
~B
�B
�B
~B
|B
wB
rB
kB
lB
eB
eB
]B
_B
^B
^B
_B
gB
jB
fB
^B
`B
WB
YB
XB
XB
YB
YB
TB
QB
LB
KB
AB
4B
'B
%B
&B
$B
#B
 B
B
B
B
B

B
	B
�B
�B
�B
�B
�B
�B
�B
�B
�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�{B	�yB	�uB	�uB	�uB	�nB	�pB	�oB	�gB	�pB	�pB	�qB	�oB	�nB	�oB	�jB	�gB	�bB	�cB	�iB	�cB	�jB	�oB	�iB	�hB	�cB	�]B	�WB	�OB	�NB	�OB	�VB	�]B	�TB	�VB	�XB	�UB	�\B	�nB	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�xB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	 B
�B
�B
	 B
�B
	 B
�B

B

B

B

B

B

B

B
B
B
B
B
B
B
B
%B
%B
&B
#B
#B
#B
+B
,B
,B
-B
.B
0B
1B
/B
0B
/B
0B
6B
6B
6B
7B
5B
=B
<B
?B
6B
6B
6B
=B
>B
>B
<B
;B
CB
CB
CB
BB
AB
FB
FB
OB
UB
SB
TB
UB
\B
ZB
[B
VB
UB
[B
bB
bB
eB
cB
fB
eB
lB
nB
nB
eB
eB
gB
fB
iB
fB
fB
mB
tB
tB
yB
rB
sB
~B
G�O�B
~B
&�B
+�B
0�B
8B
9B
BWB
K�B
P�B
U�B
[�B
aB
d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.48 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417332016080714173320160807141733  AO  ARCAADJP                                                                    20150226221308    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221308  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221308  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141733  IP                  G�O�G�O�G�O�                