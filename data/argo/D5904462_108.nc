CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-03-05T03:15:46Z AOML 3.0 creation; 2016-08-07T21:51:27Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160305031546  20160807145127  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               lA   AO  5287_9017_108                   2C  D   APEX                            6529                            072314                          846 @ך���71   @ך�}'�
@/�-�dꗍO�;1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    lA   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  Bؙ�B�ffB���B�  B�  B�  B�  B�33B�  B���C   C  C  C  C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dyy�D�3D�I�D���D�� D�fD�Y�D��fD��3D��D�33D�i�Dǰ D�fD�Y�Dډ�D�3D�3D�C3D�fD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@�Q�A(�A$(�AD(�Ad(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B	
=B
=B
=B!
=B)
=B1
=B9
=BA
=BI
=BQ
=BY
=Ba
=Bi
=Bq
=By
=B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BąBȅB̅BЅBԅB��B��B�Q�B�B�B�B��B��RB��B�Q�C B�CB�CB�CB�C(�C
B�CB�CB�CB�CB�CB�CB�CB�CB�CB�CB�C B�C"B�C$\)C&B�C(B�C*B�C,B�C.B�C0B�C2B�C4B�C6B�C8B�C:B�C<B�C>B�C@B�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CPB�CRB�CTB�CVB�CXB�CZB�C\B�C^B�C`B�CbB�CdB�CfB�ChB�CjB�ClB�Cn(�CpB�CrB�CtB�CvB�CxB�CzB�C|B�C~B�C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�>Dy�>D��D�Q�D���D��RD��D�a�D���D���D�!�D�;�D�q�DǸRD��D�a�Dڑ�D໅D��D�K�D�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ĜA͉7A͋DA��A�ƨA̝�A̓uȦ+A�|�A�z�A�v�A�t�A�p�A�n�A�l�A�p�A�n�A�l�A�l�A�l�A�p�A�r�A�r�A�p�A�p�A�p�A�l�A�l�A�l�A�jA�jA�hsA�l�A�jA�jA�hsA�dZA�dZA�ffA�ffA�bNA�bNA�`BA�bNA�ffA�hsA�n�A�v�A�S�A�{A˛�A��A�1'A��A�AļjA�x�A���A�bA�ffA���A�~�A��A��
A�oA�9XA��A�A�%A�A�1'A�XA�z�A�/A�ffA�1A��A�?}A��TA�XA�XA��A���A��
A��uA�ffA�  A���A�G�A�~�A���A��7A��A��A�\)A�+A��9A�VA���A�+A���A��
A��-A�%A���A��A�1'A��!A��A��+A�r�Axv�As�TAqO�Ah��Ae;dAaXA[�^AX �AVbAT5?AQ��AP$�AMhsAJ1AG��AD�A@Q�A<�A9G�A7�A5?}A2=qA0�DA/VA.=qA-\)A+�A*$�A)�A)7LA)x�A)��A*^5A*bNA*��A*ĜA*�\A)��A(ȴA(z�A($�A'��A'�A'G�A'hsA'+A%�;A%�A$�DA#S�A"�/A!��A��A�A33AffA�FA�uAr�A`BA��A�A��AoA�AffA�yA;dA��A�
A�-AE�A��AG�A��A��A\)A\)AS�A��A�/A�A
=A+A
^5A/AoA
�9A	��A	�A	
=A�A�9A	x�AJAAƨA+A�`A��A�A��AS�A|�A�A"�@��
@�=q@�I�@��@��@�1'@�%@�bN@��;@�-@��@�1@�dZ@�o@�v�@�@�bN@�dZ@��@�7@�O�@�/@�`B@�j@�1'@�I�@�@��@�`B@�^@�9@�+@��@@��#@���@�\)@�C�@�M�@�Ĝ@��
@��y@�V@�G�@��@�@�5?@�v�@�n�@�-@��/@���@�@��`@�%@�V@�b@�ȴ@ޗ�@�M�@��@ݑh@݁@�?}@�9X@�b@�V@�?}@���@���@��@���@��@��@ٺ^@�@җ�@�5?@щ7@� �@�l�@�K�@�=q@͉7@�hs@ΰ!@�hs@��@̬@˶F@��m@̋D@�S�@�M�@�hs@��`@�Ĝ@ȼj@ȴ9@ț�@ǶF@Ə\@���@�z�@���@å�@Õ�@�dZ@�+@�ff@�@��@���@�X@���@�1@��F@�C�@��!@��#@���@��@�1'@��@�K�@��@��\@��@�hs@���@��`@���@�  @�33@��@�~�@�=q@���@���@�x�@�7L@�%@��j@��
@���@�|�@�l�@�S�@�S�@�33@�
=@��!@�V@��@���@��h@��@��/@�Ĝ@���@�A�@���@���@�K�@���@�ȴ@��@�ȴ@���@��+@�^5@���@���@���@�7L@��@�b@�ƨ@���@��@�33@�o@��H@��@��+@�=q@��@���@���@��@�hs@�O�@�G�@�/@��/@�r�@�I�@�b@���@��@�l�@�33@��@��@��+@�M�@�$�@�n�@�x�@��@�1'@�ƨ@�"�@��@��\@��T@��@�O�@��@��`@���@��@�Q�@���@�K�@���@��+@�^5@�M�@�@��@��@��`@���@�A�@��@���@�33@���@�-@�p�@���@�r�@�9X@��@���@��y@��H@��@�ȴ@���@�ff@�5?@�$�@��@�x�@�&�@��@��@��@��@���@��`@��/@���@��@�9X@���@�33@��@�@�1'@��w@�5?@|�D@t9X@jn�@a��@W�@QX@J�@@��@8A�@/\)@*��@$�D@I�@ �@(�@ �@�@3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�ĜA͉7A͋DA��A�ƨA̝�A̓uȦ+A�|�A�z�A�v�A�t�A�p�A�n�A�l�A�p�A�n�A�l�A�l�A�l�A�p�A�r�A�r�A�p�A�p�A�p�A�l�A�l�A�l�A�jA�jA�hsA�l�A�jA�jA�hsA�dZA�dZA�ffA�ffA�bNA�bNA�`BA�bNA�ffA�hsA�n�A�v�A�S�A�{A˛�A��A�1'A��A�AļjA�x�A���A�bA�ffA���A�~�A��A��
A�oA�9XA��A�A�%A�A�1'A�XA�z�A�/A�ffA�1A��A�?}A��TA�XA�XA��A���A��
A��uA�ffA�  A���A�G�A�~�A���A��7A��A��A�\)A�+A��9A�VA���A�+A���A��
A��-A�%A���A��A�1'A��!A��A��+A�r�Axv�As�TAqO�Ah��Ae;dAaXA[�^AX �AVbAT5?AQ��AP$�AMhsAJ1AG��AD�A@Q�A<�A9G�A7�A5?}A2=qA0�DA/VA.=qA-\)A+�A*$�A)�A)7LA)x�A)��A*^5A*bNA*��A*ĜA*�\A)��A(ȴA(z�A($�A'��A'�A'G�A'hsA'+A%�;A%�A$�DA#S�A"�/A!��A��A�A33AffA�FA�uAr�A`BA��A�A��AoA�AffA�yA;dA��A�
A�-AE�A��AG�A��A��A\)A\)AS�A��A�/A�A
=A+A
^5A/AoA
�9A	��A	�A	
=A�A�9A	x�AJAAƨA+A�`A��A�A��AS�A|�A�A"�@��
@�=q@�I�@��@��@�1'@�%@�bN@��;@�-@��@�1@�dZ@�o@�v�@�@�bN@�dZ@��@�7@�O�@�/@�`B@�j@�1'@�I�@�@��@�`B@�^@�9@�+@��@@��#@���@�\)@�C�@�M�@�Ĝ@��
@��y@�V@�G�@��@�@�5?@�v�@�n�@�-@��/@���@�@��`@�%@�V@�b@�ȴ@ޗ�@�M�@��@ݑh@݁@�?}@�9X@�b@�V@�?}@���@���@��@���@��@��@ٺ^@�@җ�@�5?@щ7@� �@�l�@�K�@�=q@͉7@�hs@ΰ!@�hs@��@̬@˶F@��m@̋D@�S�@�M�@�hs@��`@�Ĝ@ȼj@ȴ9@ț�@ǶF@Ə\@���@�z�@���@å�@Õ�@�dZ@�+@�ff@�@��@���@�X@���@�1@��F@�C�@��!@��#@���@��@�1'@��@�K�@��@��\@��@�hs@���@��`@���@�  @�33@��@�~�@�=q@���@���@�x�@�7L@�%@��j@��
@���@�|�@�l�@�S�@�S�@�33@�
=@��!@�V@��@���@��h@��@��/@�Ĝ@���@�A�@���@���@�K�@���@�ȴ@��@�ȴ@���@��+@�^5@���@���@���@�7L@��@�b@�ƨ@���@��@�33@�o@��H@��@��+@�=q@��@���@���@��@�hs@�O�@�G�@�/@��/@�r�@�I�@�b@���@��@�l�@�33@��@��@��+@�M�@�$�@�n�@�x�@��@�1'@�ƨ@�"�@��@��\@��T@��@�O�@��@��`@���@��@�Q�@���@�K�@���@��+@�^5@�M�@�@��@��@��`@���@�A�@��@���@�33@���@�-@�p�@���@�r�@�9X@��@���@��y@��H@��@�ȴ@���@�ff@�5?@�$�@��@�x�@�&�@��@��@��@��@���@��`@��/@���@��@�9X@���@�33@��G�O�@�1'@��w@�5?@|�D@t9X@jn�@a��@W�@QX@J�@@��@8A�@/\)@*��@$�D@I�@ �@(�@ �@�@3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
ƨB
ÖB
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
B
B
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
��B
B
ĜB
ƨB
ȴB
��B
�NBJBE�Bx�B�
B�BG�BW
Bq�B�B�=B��B��B��B��B�dB�qB�qB�FB��B��B��B�oB�JB�+B�Bz�Bw�Bs�Bp�Bp�Bm�BffB_;BR�B@�B6FB9XBF�BQ�BYB`BB]/BT�BJ�B49BB�TB��BB�^B�'B��B�hBaHBL�B8RB#�BJB
�TB
ĜB
��B
cTB	�B	B	��B	u�B	\)B	I�B	33B	!�B	�B	PB	B��B��B�B�TB��B��BĜB�qB�LB�-B�-B�?B�9B�3B�9B�-B�RBŢB�;B	B	JB	$�B	1'B	I�B	N�B	VB	[#B	YB	VB	S�B	W
B	ffB	q�B	t�B	v�B	r�B	n�B	k�B	hsB	e`B	dZB	n�B	u�B	}�B	|�B	x�B	w�B	u�B	r�B	o�B	l�B	iyB	hsB	jB	x�B	�B	��B	�3B	�dB	�wB	�qB	�qB	�wB	ÖB	��B	B	�?B	�}B	��B	ŢB	�}B	�RB	�'B	�B	ÖB	ƨB	B	�}B	ŢB	�}B	�^B	ŢB	��B	ÖB	�jB	�3B	�B	�B	�!B	�!B	�B	��B	�B	��B	��B	��B	�uB	�\B	�PB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�hB	�\B	�uB	��B	��B	��B	��B	��B	��B	�B	�-B	�9B	�wB	ÖB	ǮB	ŢB	ĜB	ŢB	ĜB	�}B	�XB	�RB	�LB	�XB	�LB	�RB	�XB	�qB	�dB	�^B	�?B	�^B	�jB	�dB	�}B	��B	��B	ĜB	ƨB	ǮB	ĜB	��B	��B	��B	�}B	�wB	�wB	�wB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ƨB	�LB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�9B	�-B	�XB	�wB	�RB	�9B	�?B	�^B	�dB	�dB	�dB	�dB	�wB	�jB	�^B	�XB	�^B	�^B	�^B	�^B	�dB	�wB	�}B	�}B	�}B	��B	B	ÖB	ÖB	ÖB	ǮB	ƨB	ĜB	ŢB	ĜB	ŢB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�
B	�
B	�B	�B	�B	�/B	�/B	�)B	�)B	�/B	�5B	�5B	�5B	�/B	�5B	�5B	�5B	�;B	�BB	�TB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�mB	�`B	�`B	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
%B
%B
%B
+B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
DB
JB
PB
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
hB
oB
uB
uB
�B
�B
%�B
'�B
)�B
0!B
7LB
>wB
D�B
J�B
Q�B
XB
aHB
e`B
k�B
r�B
u�B
x�B
{�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
��B
˶B
ʰB
ƒB
ÂB
�uB
�uB
�yB
�qB
�sB
�qB
�oB
�rB
�qB
�vB
�yB
�vB
�rB
�rB
�yB
�{B
�|B
�}B
�vB
�vB
�yB
�yB
�vB
�tB
�rB
�rB
�yB
�yB
�yB
�uB
�vB
�tB
�qB
�tB
�rB
�vB
�yB
�yB
�|B
ČB
ƖB
ȡB
��B
�=B9BE�Bx�B��BqBG�BV�Bq�B�B�'B�rB��B��B��B�LB�[B�XB�1B��B��B�xB�XB�3B�B��Bz�Bw�Bs�Bp�Bp�Bm|BfPB_$BR�B@iB60B9@BF�BQ�BX�B`,B]BT�BJ�B4#B�B�:BͺB�wB�FB�B��B�OBa/BL�B88B#�B2B
�;B
ĄB
��B
c?B	�yB	B	��B	u�B	\B	I�B	3(B	!�B	yB	DB	B��B��B�B�JB��B��BĒB�iB�BB�%B�%B�5B�/B�'B�1B�$B�HBŗB�0B	�B	:B	$�B	1B	I�B	N�B	U�B	[B	YB	U�B	S�B	V�B	fTB	q�B	t�B	v�B	r�B	n�B	ksB	hbB	eNB	dIB	n�B	u�B	}�B	|�B	x�B	w�B	u�B	r�B	o�B	lxB	ieB	h`B	jlB	x�B	�B	��B	�B	�MB	�aB	�]B	�YB	�bB	ÀB	ʫB	�yB	�+B	�eB	˱B	ōB	�gB	�;B	�B	�B	�B	ƒB	�yB	�fB	ŌB	�fB	�HB	ōB	��B	�}B	�TB	�B	�B	��B	�B	�
B	��B	��B	��B	��B	��B	�qB	�_B	�FB	�<B	�dB	�}B	��B	��B	��B	��B	��B	�|B	�jB	�kB	�kB	�fB	�RB	�EB	�_B	�iB	�B	��B	��B	��B	��B	��B	�B	�!B	�cB	�|B	ǘB	ŉB	ąB	ŊB	ăB	�fB	�@B	�:B	�3B	�?B	�6B	�;B	�BB	�ZB	�KB	�HB	�'B	�EB	�SB	�MB	�dB	�mB	�rB	ĂB	ƒB	ǗB	āB	�rB	�qB	�lB	�cB	�_B	�]B	�`B	�eB	�kB	ʫB	ˮB	̳B	ͺB	��B	��B	��B	˯B	ƏB	�4B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�B	�?B	�^B	�:B	�!B	�'B	�EB	�LB	�MB	�NB	�MB	�_B	�SB	�FB	�>B	�DB	�EB	�EB	�FB	�KB	�aB	�dB	�dB	�eB	�rB	�wB	�|B	�{B	�}B	ǔB	ƐB	ăB	ňB	ăB	ŉB	ǕB	țB	ȚB	ʧB	̵B	̵B	͸B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�&B	�9B	�?B	�FB	�GB	�FB	�KB	�LB	�JB	�KB	�RB	�GB	�HB	�KB	�SB	�cB	�mB	�tB	�qB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
 B
B
B
B
B
B
	B
	B
	B
	B
	B

 B

!B
)B
)B
)B
.B
4B
9B
9B
<B
:B
>B
AB
AB
AB
BB
JB
NB
SB
YG�O�B
dB
�B
%�B
'�B
)�B
0B
7.B
>YB
D�B
J�B
Q�B
W�B
a*B
eCB
keB
r�B
u�B
x�B
{�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.26 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451272016080714512720160807145127  AO  ARCAADJP                                                                    20160305031546    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160305031546  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160305031546  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145127  IP                  G�O�G�O�G�O�                