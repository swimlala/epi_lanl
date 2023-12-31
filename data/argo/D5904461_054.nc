CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-06-14T02:15:49Z AOML 3.0 creation; 2016-08-07T21:36:36Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150614021549  20160807143636  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               6A   AO  5286_8897_054                   2C  D   APEX                            6531                            072314                          846 @�X: ���1   @�X:��?�@1���l�D�c�G�z�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    6A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh��Bo��Bw��B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtY�Dy��D�	�D�Y�D���D��fD��D�S3D�vfD��3D��D�P D��fD�ٚD��D�,�DږfD��3D��D�L�D�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�{@�{A
=A#
=AC
=Ac
=A��A��A��A��A��AхA�A�B BBBB B(B0B8B@BHBPBXB`Bi�\Bp\)Bx\)B�aHB�aHB�aHB�aHB�aHB�aHB�aHB��{B�aHB�.B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�.B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC 0�C0�C0�C0�C0�C
0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C 0�C"0�C$0�C&0�C(0�C*0�C,0�C.0�C00�C20�C40�C60�C80�C:0�C<0�C>0�C@0�CB0�CD0�CF0�CH0�CJ0�CL0�CN0�CP0�CR0�CTJ>CV0�CX0�CZ0�C\0�C^0�C`0�Cb0�Cd0�Cf0�Ch0�Cj0�Cl0�Cn0�Cp0�Cr0�Ct0�Cv0�Cx0�Cz0�C|0�C~0�C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!)D!�)D")D"�)D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dte�Dy��D��D�_�D���D��zD�"�D�YGD�|zD��GD��D�VD��zD�߮D��D�2�DڜzD��GD��D�R�D�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�=qA�1'A�1'A�/A�/A�1'A�7LA�7LA�33A�1'A�/A�-A�-A�-A�(�A�(�A�(�A�&�A� �A��A�{A��A�$�A���A�$�A���A·+A��A�z�A���A̟�A̲-Ạ�Ȁ\A���A�A�A�A�33A�1A�
=Aŕ�A�^5A�v�A�jA�v�A�n�A���A�~�A��A��\A�oA�\)A�l�A��#A��+A�hsA��A��uA�ĜA���A���A��A�hsA��A��#A�z�A���A�=qA�
=A��A���A���A�  A��7A���A�{A�I�A�O�A�7LA�+A��#A�K�A���A��yA�dZA��A�33A�7LA� �A�t�A���A�~�A�x�A��mA��PA�JA�bA��DA~��A{�AuS�Aq�ApffAn�9Ai��Af�Ad�+Ac7LAa�A\$�AY��AWt�AU��ATjAQ�AP��ALv�AJ�9AJ�AH�ADv�AAdZA@��A=�
A;�#A:��A9O�A7�;A6��A5�A4�yA4bA3C�A1�TA0��A/�A.jA,�A,v�A+��A+�A*VA)�wA)"�A(1A&$�A$JA"��A!l�A�A�A�TA33A��AƨAz�A��AK�Az�A�mAoA��A�
A;dA�uA1AhsA~�A�A?}AVA�DA+A��AQ�A�A�TAx�A�HA1'AG�A
��A
�A
5?A	�;A	�7A��AA�AXA�/Al�Ap�AȴA
=A (�@�dZ@�n�@���@�Q�@�M�@���@���@�7L@���@�l�@��`@�bN@���@��@���@�Ĝ@��y@��#@�?}@�9@�j@�Z@�1'@�K�@�=q@�{@�&�@�+@��@�J@�bN@���@�?}@��T@�J@�%@��;@�dZ@ۅ@�dZ@�\)@ڟ�@�x�@�r�@���@Ӯ@ӶF@�  @��;@�ƨ@���@���@���@���@��;@��;@��
@�t�@�C�@�v�@��@Ѻ^@д9@�z�@�r�@�1@�S�@Η�@�E�@Ͳ-@�G�@�V@���@�bN@�I�@�9X@�1'@� �@���@˶F@�K�@��H@���@ʏ\@��@���@�p�@�%@ț�@�j@�Z@��;@ǅ@Ɵ�@�{@���@Ų-@�X@��`@�I�@���@��
@þw@ÍP@�S�@�;d@��@�ȴ@�ff@��^@��@��@�%@��@�1@���@��@���@��@�M�@���@�x�@�?}@���@�r�@�(�@�ƨ@���@��@��H@��R@�ff@��-@�G�@���@���@��@�j@�Z@�1@��P@�\)@�ff@���@��;@�33@���@��@���@���@���@��
@��@��@��@��@��P@�dZ@�;d@�o@�o@�=q@��D@���@��/@���@�ƨ@�1@��@�ȴ@���@��#@��-@�hs@�/@���@�r�@�9X@��@�1'@�A�@�Q�@�bN@��D@�j@�bN@�I�@� �@���@�ƨ@���@�t�@�@�n�@�J@��@��^@�x�@�`B@�G�@�%@��`@�V@��@��@��@� �@��w@�|�@�33@��@���@�=q@���@���@�O�@�%@���@��9@�r�@�Z@�I�@�b@�ƨ@�S�@�+@��@���@�-@���@��T@�p�@�7L@�V@�V@��@���@���@�bN@�bN@� �@�1@�;d@�o@��@�;d@��@��R@���@���@�v�@��T@�hs@���@��j@��9@�I�@�dZ@�C�@�o@��R@�5?@�J@��^@���@�hs@�G�@��@���@�V@��/@�r�@�bN@�A�@�(�@��@�  @��w@�\)@��@�ȴ@�v�@�M�@�-@��@�J@���@���@�x�@�V@��j@�bN@�E�@�E�@���@~�R@u@l�j@c�@Y7L@P��@G�@AG�@;@1G�@*n�@$�j@ ��@�@&�@�m@l�@(�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�=qA�1'A�1'A�/A�/A�1'A�7LA�7LA�33A�1'A�/A�-A�-A�-A�(�A�(�A�(�A�&�A� �A��A�{A��A�$�A���A�$�A���A·+A��A�z�A���A̟�A̲-Ạ�Ȁ\A���A�A�A�A�33A�1A�
=Aŕ�A�^5A�v�A�jA�v�A�n�A���A�~�A��A��\A�oA�\)A�l�A��#A��+A�hsA��A��uA�ĜA���A���A��A�hsA��A��#A�z�A���A�=qA�
=A��A���A���A�  A��7A���A�{A�I�A�O�A�7LA�+A��#A�K�A���A��yA�dZA��A�33A�7LA� �A�t�A���A�~�A�x�A��mA��PA�JA�bA��DA~��A{�AuS�Aq�ApffAn�9Ai��Af�Ad�+Ac7LAa�A\$�AY��AWt�AU��ATjAQ�AP��ALv�AJ�9AJ�AH�ADv�AAdZA@��A=�
A;�#A:��A9O�A7�;A6��A5�A4�yA4bA3C�A1�TA0��A/�A.jA,�A,v�A+��A+�A*VA)�wA)"�A(1A&$�A$JA"��A!l�A�A�A�TA33A��AƨAz�A��AK�Az�A�mAoA��A�
A;dA�uA1AhsA~�A�A?}AVA�DA+A��AQ�A�A�TAx�A�HA1'AG�A
��A
�A
5?A	�;A	�7A��AA�AXA�/Al�Ap�AȴA
=A (�@�dZ@�n�@���@�Q�@�M�@���@���@�7L@���@�l�@��`@�bN@���@��@���@�Ĝ@��y@��#@�?}@�9@�j@�Z@�1'@�K�@�=q@�{@�&�@�+@��@�J@�bN@���@�?}@��T@�J@�%@��;@�dZ@ۅ@�dZ@�\)@ڟ�@�x�@�r�@���@Ӯ@ӶF@�  @��;@�ƨ@���@���@���@���@��;@��;@��
@�t�@�C�@�v�@��@Ѻ^@д9@�z�@�r�@�1@�S�@Η�@�E�@Ͳ-@�G�@�V@���@�bN@�I�@�9X@�1'@� �@���@˶F@�K�@��H@���@ʏ\@��@���@�p�@�%@ț�@�j@�Z@��;@ǅ@Ɵ�@�{@���@Ų-@�X@��`@�I�@���@��
@þw@ÍP@�S�@�;d@��@�ȴ@�ff@��^@��@��@�%@��@�1@���@��@���@��@�M�@���@�x�@�?}@���@�r�@�(�@�ƨ@���@��@��H@��R@�ff@��-@�G�@���@���@��@�j@�Z@�1@��P@�\)@�ff@���@��;@�33@���@��@���@���@���@��
@��@��@��@��@��P@�dZ@�;d@�o@�o@�=q@��D@���@��/@���@�ƨ@�1@��@�ȴ@���@��#@��-@�hs@�/@���@�r�@�9X@��@�1'@�A�@�Q�@�bN@��D@�j@�bN@�I�@� �@���@�ƨ@���@�t�@�@�n�@�J@��@��^@�x�@�`B@�G�@�%@��`@�V@��@��@��@� �@��w@�|�@�33@��@���@�=q@���@���@�O�@�%@���@��9@�r�@�Z@�I�@�b@�ƨ@�S�@�+@��@���@�-@���@��T@�p�@�7L@�V@�V@��@���@���@�bN@�bN@� �@�1@�;d@�o@��@�;d@��@��R@���@���@�v�@��T@�hs@���@��j@��9@�I�@�dZ@�C�@�o@��R@�5?@�J@��^@���@�hs@�G�@��@���@�V@��/@�r�@�bN@�A�@�(�@��@�  @��w@�\)@��@�ȴ@�v�@�M�@�-@��@�J@���@���@�x�@�V@��jG�O�@�E�@�E�@���@~�R@u@l�j@c�@Y7L@P��@G�@AG�@;@1G�@*n�@$�j@ ��@�@&�@�m@l�@(�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	y�B	z�B	y�B	z�B	z�B	z�B	y�B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	{�B	{�B	{�B	|�B	}�B	��B	�B	�dB	��B	�B
%�B
��B
�5B�BA�B[#Bq�B|�B~�B�FB�mB\B;dBR�B^5Bo�Bx�By�B�=By�B�B�FB�FB�Bv�BgmB[#B"�BP�B�+B}�B��B�B�B��B�\B�=B�Bw�B��B�uB�B|�Bp�Bk�Bp�Bo�BgmBI�B!�B
=B��B�B�NBĜB�!B�'B�B��B�7Br�B`BB7LB
�B
ÖB
�\B
`BB
A�B
!�B	��B	�#B	ÖB	��B	�%B	�%B	{�B	`BB	S�B	H�B	?}B	2-B	�B	hB	%B��B�B�yB�ZB�#B��B��BǮB�}B�XB�?B�B��B��B�!B�LB�9B�-B�^B�dB�LB�FB�qBĜB��B�dB�^B�dBB��B�/B�5B�)B�#B��B��B��BǮBǮBȴBɺB��B��B��B��B��B��B��B��B��B��BȴBɺB��B��B��B�B�B�B�B�
B�B�B�B�)B�5B�)B�#B�/B�/B�/B�;B�;B�5B�5B�BB�NB�ZB�NB�;B�sB�B�TB�ZB�fB�B�B��B�B�B�B�B�yB�mB�B�B�B�yB�fB�mB�sB�sB�sB�mB�mB�mB�mB�fB�fB�fB�sB�B�B�B�B�B�B�B��B��B�B��B	  B	B	+B	1B	B��B��B��B��B��B	B	1B	VB	bB	hB	hB	uB	{B	�B	�B	�B	�B	"�B	#�B	)�B	)�B	)�B	0!B	9XB	:^B	<jB	=qB	@�B	B�B	D�B	G�B	G�B	H�B	J�B	L�B	M�B	N�B	Q�B	XB	[#B	^5B	aHB	bNB	cTB	dZB	ffB	ffB	ffB	iyB	jB	n�B	q�B	r�B	t�B	u�B	x�B	|�B	~�B	~�B	� B	�B	�B	�B	�%B	�%B	�%B	�DB	�PB	�VB	�VB	�\B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�9B	�LB	�XB	�^B	�XB	�XB	�^B	�dB	�qB	�wB	�wB	�qB	�jB	�jB	�jB	�jB	�qB	B	��B	ÖB	ƨB	ŢB	B	�}B	�}B	�}B	�wB	��B	ŢB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�
B	�#B	�#B	�#B	�)B	�/B	�/B	�5B	�;B	�BB	�HB	�NB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
+B
%B
DB
uB
�B
 �B
&�B
-B
33B
:^B
A�B
F�B
L�B
S�B
ZB
`BB
cTB
e`B
k�B
o�B
u�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	y�B	z�B	y�B	z�B	z�B	z�B	y�B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	z�B	{�B	{�B	{�B	|�B	}�B	��B	�B	�bB	��B	�B
%�B
��B
�*B�BAzB[Bq�B|�B~�B�8B�`BLB;UBR�B^'Bo�Bx�By�B�,By�B�B�7B�9B� Bv�Bg\B[B"�BP�B�B}�B��B�B�B��B�OB�-B� Bw�B��B�bB�B|�Bp�BksBp�Bo�Bg^BI�B!�B
*B��B�tB�?BĊB�B�B��B��B�&Br�B`1B7=B
�B
ÈB
�MB
`5B
A~B
!�B	��B	�B	ÊB	��B	�B	�B	{�B	`;B	S�B	H�B	?yB	2'B	�B	dB	#B��B�B�xB�[B�B��B��BǫB�zB�VB�?B�B��B��B�!B�KB�9B�+B�\B�cB�KB�DB�pBĜB��B�dB�\B�`BB��B�,B�1B�$B�B��B��BʿBǩBǪBȰBɷB��B��B��B��B��B��B��B��B��BʿBȯBɵB��B��B��B�B� B� B� B�B��B�B�B�#B�/B�#B�B�(B�+B�*B�5B�6B�/B�/B�>B�JB�VB�JB�5B�lB�B�NB�TB�_B�~B�B��B�B�B�B�}B�tB�eB�yB�B�B�rB�`B�eB�jB�mB�lB�dB�fB�fB�eB�]B�^B�\B�kB�wB�xB�B�B�~B�}B�B��B��B�B��B��B	B	"B	)B	B��B��B��B��B��B	B	*B	MB	XB	]B	_B	lB	sB	|B	�B	�B	�B	"�B	#�B	)�B	)�B	)�B	0B	9LB	:RB	<`B	=hB	@{B	B�B	D�B	G�B	G�B	H�B	J�B	L�B	M�B	N�B	Q�B	XB	[B	^'B	a<B	b@B	cFB	dNB	fWB	fZB	fXB	ilB	jrB	n�B	q�B	r�B	t�B	u�B	x�B	|�B	~�B	~�B	�B	��B	�	B	�B	�B	�B	�B	�7B	�BB	�GB	�EB	�KB	�YB	�_B	�rB	�yB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�(B	�:B	�DB	�NB	�HB	�HB	�NB	�TB	�`B	�gB	�fB	�`B	�^B	�^B	�ZB	�XB	�`B	�~B	�zB	ÄB	ƘB	œB	�}B	�lB	�kB	�nB	�iB	�rB	ŕB	ǟB	ɪB	ɩB	˷B	̺B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	� B	�$B	�*B	�0B	�7B	�<B	�;B	�BB	�CB	�JB	�HB	�GB	�OB	�OB	�PB	�OB	�UB	�ZB	�^B	�\B	�SB	�WB	�TB	�^B	�[B	�\B	�aB	�cB	�iB	�fB	�gB	�hB	�mB	�tB	�yB	�lB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
 B
B
 B
B
B
B
B
G�O�B
B
4B
cB
{B
 �B
&�B
,�B
3"B
:JB
AuB
F�B
L�B
S�B
ZB
`/B
c?B
eLB
koB
o�B
u�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.19 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436362016080714363620160807143636  AO  ARCAADJP                                                                    20150614021549    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150614021549  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150614021549  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143636  IP                  G�O�G�O�G�O�                