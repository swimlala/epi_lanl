CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:33Z AOML 3.0 creation; 2016-06-01T00:08:14Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230833  20160531170814  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               7A   AO  4055_7112_055                   2C  D   APEX                            5374                            041511                          846 @ַ�����1   @ַ��@9ѩ��l��c�I�^1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    7A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffBffB��B  B   B(  B0  B8  B@  BH  BPffBXffB_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>y�D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dys3D��D�VfD�� D�� D�  D�9�D�� D�� D�fD�S3D�� D�� D�  D�33D�i�D��fD��D�L�D�l�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�{@�{A
=A#
=AC
=Ac
=A��A��A��A��A��AхA�A�B(�B	(�B\)BB B(B0B8B@BHBQ(�BY(�B`\)BhBpBxB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC 0�C0�C0�C0�C0�C
0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C 0�C"0�C$0�C&0�C(0�C*0�C,0�C.0�C00�C20�C40�C60�C80�C:0�C<0�C>0�C@0�CB0�CD0�CF0�CH0�CJ0�CL0�CN0�CP0�CR0�CT0�CV0�CX0�CZ0�C\J>C^0�C`0�Cb0�Cd0�Cf0�Ch0�Cj0�Cl0�Cn0�Cp0�Cr0�Ct0�Cv0�Cx0�Cz0�C|0�C~0�C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!)D!�)D")D"�)D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>��D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP)DP�)DQ�DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dtx�Dy\D�"�D�\zD��D��D�D�?�D��D��D�zD�YGD��D��D�D�9GD�o�D��zD��D�R�D�r�D��G11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aݣ�A�x�A�7LA�M�A�dZA��Aٰ!A�l�A�C�A��`Aغ^A�l�A���A�Q�A���A���A԰!A���A� �A�%A���A�M�A�E�AčPAÓuA�l�A���A���A���A��A�+A�ƨA�ƨA�ffA��A��#A���A��FA��A��+A��9A���A�;dA�{A��9A��A�1'A���A�x�A��A���A�ȴA�~�A��A���A��A�ƨA�33A�-A���A�v�A�l�A�jA�=qA��jA�XA�C�A�5?A�&�A��mA�z�A�A�A���A��PA�jA���A�hsA��TA���A�K�A���A�|�A��A�bNA�z�A��A���A�v�A�=qA���A��A�ĜA��FA�ĜA��\A�O�A�Q�A���A�33A�S�A���A�(�A��7A�oA��+A��yA��A���A��A�v�A~��A{t�AwXAr�RAm�Ah�uAf��AfA�Ae�hAdz�Abn�A]33A[�AYt�AX�jAW��AV��AT��AQ��AOS�AM�-AL��ALbNAK�AKAI��AIK�AH��AF�jAF5?AD��AC�AChsABbAA?}A?�A?"�A>�A<�A;ƨA:�A8��A89XA8{A7�#A7��A7�A7K�A5��A3\)A1��A1�A0��A/�mA.bA,ȴA+��A+|�A*��A*n�A)
=A'��A'��A'K�A&�!A&VA%A$�A$�RA$E�A#%A" �A �A {A�7AoA�A�AC�AA�A��A��A�A�7A
=A��A��A|�A�A�A�AS�Av�A�^A�!AVA�AXA�-A�A
��A	�;A��A �A�A�jA9XA�PA��AA�hA ��@�E�@�hs@���@�Ĝ@�I�@��P@�=q@��h@�/@��@�?}@��y@�X@� �@��@� �@�V@�7L@��m@��@�ff@���@���@�5?@���@��#@�@�bN@�ƨ@���@�|�@ى7@�r�@�K�@���@���@�=q@�\)@�O�@�bN@�b@�\)@�{@��@ȴ9@���@�"�@�/@�9X@Õ�@\@�x�@��D@�"�@�v�@�O�@��
@�n�@��D@�S�@���@�J@���@��@�A�@���@���@�(�@�X@���@��D@�Q�@� �@� �@�  @�|�@�ȴ@�n�@�J@�bN@�|�@�33@���@�-@��#@���@��h@���@��
@�|�@�|�@�\)@�+@�
=@��@��@�|�@�C�@��@�
=@���@��@��y@��@���@��R@��+@��@���@�hs@�?}@�%@�j@���@��@��@���@��h@���@��m@�ƨ@��@�
=@���@�n�@�$�@��@��w@��F@���@���@�t�@���@��+@�{@���@���@�`B@�%@���@��j@�I�@��;@�t�@�C�@�33@��@���@���@�v�@�$�@�{@��#@��h@�&�@��/@�bN@�A�@�A�@�Q�@�A�@�9X@�(�@��@��;@�t�@�"�@���@��y@��!@�-@�J@�X@�Ĝ@��@��u@�j@���@�33@���@���@��@���@�7L@���@��9@�1'@���@���@�C�@��@��y@���@���@�V@�M�@�5?@�@���@�G�@�V@���@��9@���@��@�j@�A�@�(�@�(�@��@�@��@��@l�@\)@+@~��@}��@}��@}`B@}�@}V@|��@|�@|�j@|Z@|�@{�F@{�@{�@{t�@{dZ@z�@z�!@z�\@z~�@z^5@z=q@y�#@y7L@x�`@x��@x��@xbN@xA�@x �@xb@w�w@w|�@v��@vV@v5?@v{@u�@u�h@t��@t1@s@r�!@rM�@r=q@r=q@q�@q��@q�7@q7L@nV@f��@`bN@X �@M�h@FE�@B-@=/@9��@1�^@)G�@#ƨ@�P@��@J@�@��@
=@	�7@��@�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aݣ�A�x�A�7LA�M�A�dZA��Aٰ!A�l�A�C�A��`Aغ^A�l�A���A�Q�A���A���A԰!A���A� �A�%A���A�M�A�E�AčPAÓuA�l�A���A���A���A��A�+A�ƨA�ƨA�ffA��A��#A���A��FA��A��+A��9A���A�;dA�{A��9A��A�1'A���A�x�A��A���A�ȴA�~�A��A���A��A�ƨA�33A�-A���A�v�A�l�A�jA�=qA��jA�XA�C�A�5?A�&�A��mA�z�A�A�A���A��PA�jA���A�hsA��TA���A�K�A���A�|�A��A�bNA�z�A��A���A�v�A�=qA���A��A�ĜA��FA�ĜA��\A�O�A�Q�A���A�33A�S�A���A�(�A��7A�oA��+A��yA��A���A��A�v�A~��A{t�AwXAr�RAm�Ah�uAf��AfA�Ae�hAdz�Abn�A]33A[�AYt�AX�jAW��AV��AT��AQ��AOS�AM�-AL��ALbNAK�AKAI��AIK�AH��AF�jAF5?AD��AC�AChsABbAA?}A?�A?"�A>�A<�A;ƨA:�A8��A89XA8{A7�#A7��A7�A7K�A5��A3\)A1��A1�A0��A/�mA.bA,ȴA+��A+|�A*��A*n�A)
=A'��A'��A'K�A&�!A&VA%A$�A$�RA$E�A#%A" �A �A {A�7AoA�A�AC�AA�A��A��A�A�7A
=A��A��A|�A�A�A�AS�Av�A�^A�!AVA�AXA�-A�A
��A	�;A��A �A�A�jA9XA�PA��AA�hA ��@�E�@�hs@���@�Ĝ@�I�@��P@�=q@��h@�/@��@�?}@��y@�X@� �@��@� �@�V@�7L@��m@��@�ff@���@���@�5?@���@��#@�@�bN@�ƨ@���@�|�@ى7@�r�@�K�@���@���@�=q@�\)@�O�@�bN@�b@�\)@�{@��@ȴ9@���@�"�@�/@�9X@Õ�@\@�x�@��D@�"�@�v�@�O�@��
@�n�@��D@�S�@���@�J@���@��@�A�@���@���@�(�@�X@���@��D@�Q�@� �@� �@�  @�|�@�ȴ@�n�@�J@�bN@�|�@�33@���@�-@��#@���@��h@���@��
@�|�@�|�@�\)@�+@�
=@��@��@�|�@�C�@��@�
=@���@��@��y@��@���@��R@��+@��@���@�hs@�?}@�%@�j@���@��@��@���@��h@���@��m@�ƨ@��@�
=@���@�n�@�$�@��@��w@��F@���@���@�t�@���@��+@�{@���@���@�`B@�%@���@��j@�I�@��;@�t�@�C�@�33@��@���@���@�v�@�$�@�{@��#@��h@�&�@��/@�bN@�A�@�A�@�Q�@�A�@�9X@�(�@��@��;@�t�@�"�@���@��y@��!@�-@�J@�X@�Ĝ@��@��u@�j@���@�33@���@���@��@���@�7L@���@��9@�1'@���@���@�C�@��@��y@���@���@�V@�M�@�5?@�@���@�G�@�V@���@��9@���@��@�j@�A�@�(�@�(�@��@�@��@��@l�@\)@+@~��@}��@}��@}`B@}�@}V@|��@|�@|�j@|Z@|�@{�F@{�@{�@{t�@{dZ@z�@z�!@z�\@z~�@z^5@z=q@y�#@y7L@x�`@x��@x��@xbN@xA�@x �@xb@w�w@w|�@v��@vV@v5?@v{@u�@u�h@t��@t1@s@r�!@rM�@r=q@r=q@q�@q��@q�7@q7L@nV@f��@`bN@X �@M�h@FE�@B-@=/@9��@1�^@)G�@#ƨ@�P@��@J@�@��@
=@	�7@��@�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B��B��B�B~�B|�B}�B}�B{�Bx�Br�BhsBaHBbNBR�BA�B�B��B�`B��B�B�oB�=B~�BiyBQ�BF�B@�BgmB|�B~�Bz�Bz�B}�B}�B|�B�B�B�B�B~�By�Bl�BiyB`BB\)BYBVBP�BL�BI�BF�BE�BB�B=qB<jB5?B'�B�B�B�B�B�B\B	7B+B+BB  B��B�B�yB�ZB�BB��BɺB�jB��B��B�1Bm�BbNBP�B=qB&�BoB��B�B�ZB�?B��B� Bk�BS�BL�B6FB�B
��B
�yB
�5B
��B
ÖB
�^B
�B
��B
{�B
\)B
K�B
?}B
-B
\B	�B	ȴB	��B	�B	v�B	s�B	q�B	n�B	^5B	6FB	$�B	�B	�B	\B	1B��B�B�;B�
B��B��B��B��BȴBƨBÖB�wB�qB�jB�dB�^B�RB�FB�3B�'B�'B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B�{B�hB�VB�DB�=B�1B�%B�B�B�B� B}�B|�Bz�Bx�Bw�Bu�Br�Bo�Bl�Bk�BiyBhsBffBdZBaHB^5BZBYBXBVBS�BO�BL�BI�BH�BG�BF�BD�BB�B@�B?}B=qB;dB8RB5?B33B0!B.B,B(�B$�B"�B �B�B�B�B�B�B�B�B{B{BuBoBhBbBPBJBDB
=B	7B1B+B%B%B%B%B%B%B%B+B+B+B%B%B%BBBBBBBBBBB%B+B%B%B+B	7B
=B
=B	7BDBJBJBPBVB\BoBoBoBuB�B�B�B�B�B�B�B�B �B"�B&�B2-B49B6FB7LB8RB8RB8RB;dB>wB?}B@�BH�BJ�BN�BO�BQ�BT�BT�BVBYB_;BbNBcTBcTBe`BffBe`Br�By�B{�B}�B}�B~�B~�B~�B� B� B� B�B�B�%B�1B�7B�=B�VB�hB�oB��B��B��B��B�B�B�B�B�!B�-B�3B�qBÖBÖBÖBÖBĜBɺB��B��B��B��B��B��B�B�B�B�)B�BB�HB�HB�NB�TB�`B�fB�sB�yB�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	B	B	B	1B		7B	JB	bB	hB	oB	�B	�B	�B	"�B	#�B	'�B	,B	0!B	33B	6FB	<jB	>wB	B�B	F�B	H�B	J�B	L�B	N�B	P�B	Q�B	R�B	XB	ZB	^5B	`BB	cTB	e`B	ffB	gmB	hsB	jB	k�B	k�B	l�B	m�B	n�B	o�B	p�B	p�B	q�B	t�B	x�B	y�B	z�B	|�B	}�B	}�B	}�B	� B	�B	�B	�B	�%B	�%B	�%B	�%B	�1B	�=B	�=B	�DB	�DB	�JB	�PB	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�dB	��B	�ZB	��B
PB
�B
!�B
+B
0!B
=qB
I�B
N�B
T�B
ZB
\)B
_;B
cTB
jB
p�B
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�	B��B��B�B~�B|�B}�B}�B{�Bx�Br�BhnBaEBbLBR�BA�B�B��B�YB��B�B�eB�1B~�BipBQ�BF�B@tBg`B|�B~�Bz�Bz�B}�B}�B|�B��B�B�
B�B~�By�Bl~BiiB`8B\BYBU�BP�BL�BI�BF�BE�BB~B=aB<[B50B'�B�B�B�B�B~BKB	%BBBB��B��B�B�jB�LB�2B��BɨB�[B��B��B�!BmBb=BP�B=_B&�B]B��B�B�GB�.B�zB�BkrBS�BL�B61BpB
��B
�gB
�%B
��B
ÄB
�OB
�B
��B
{�B
\B
K�B
?pB
-B
QB	�B	ȪB	��B	�B	v�B	s�B	q�B	n�B	^-B	6AB	$�B	�B	B	XB	.B��B�B�9B�	B��B��B��B��BȴBƥBÕB�uB�nB�jB�bB�]B�NB�EB�1B�(B�$B�B�B�B�B�B�	B� B�B��B��B��B��B��B��B��B�yB�iB�SB�BB�=B�2B�&B�B�B�	B� B}�B|�Bz�Bx�Bw�Bu�Br�Bo�Bl�Bk�Bi|BhvBfhBd[BaHB^6BZBYBXBVBS�BO�BL�BI�BH�BG�BF�BD�BB�B@�B?B=tB;hB8RB5@B36B0	B.B,	B(�B$�B"�B �B�B�B�B�BvBqBiBB~ByBWBOBJBSB2BGB
%B	 BBB)BB(BBBB(B.B,BBBBBBBBBBBB�B�B$B'BB
BBB	B
%B
#B	9B+B1B1B6B>B_BqBpBrBuB�B�B�B�B�B�B�B�B �B"�B&�B2,B49B6CB7KB8QB8QB8TB;eB>vB?|B@�BH�BJ�BN�BO�BQ�BT�BT�BVBYB_9BbJBcPBcPBe\BfcBe\Br�By�B{�B}�B}�B~�B~�B~�B�B�B�B�B�B�!B�-B�2B�9B�PB�bB�iB�B��B��B��B��B��B�B�B�B�%B�)B�iBÎBÐBÏBÎBĕBɰBʻB��B��B��B��B��B��B��B�B�B�:B�>B�>B�GB�MB�TB�]B�jB�pB�tB�}B�B�B��B��B��B��B��B��B��B��B��B��B	B	B	B	B	'B		)B	@B	WB	\B	`B	uB	�B	�B	"�B	#�B	'�B	+�B	0B	3'B	6;B	<\B	>jB	B�B	F�B	H�B	J�B	L�B	N�B	P�B	Q�B	R�B	XB	ZB	^'B	`4B	cGB	eOB	fVB	g^B	hfB	jpB	kyB	kyB	l|B	m�B	n�B	o�B	p�B	p�B	q�B	t�B	x�B	y�B	z�B	|�B	}�B	}�B	}�B	�B	��B	�B	�B	�B	�B	�B	�B	�"B	�-B	�-B	�4B	�4B	�<B	�?B	�YB	�]B	�_B	�jB	�nB	�xB	�|B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�SB	��B	�HB	��B
;B
�B
!�B
*�B
0B
=\B
I�B
N�B
T�B
ZB
\B
_(B
c?B
jiB
p�B
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.19 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708142016053117081420160531170814  AO  ARCAADJP                                                                    20140721230833    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230833  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230833  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170814  IP                  G�O�G�O�G�O�                