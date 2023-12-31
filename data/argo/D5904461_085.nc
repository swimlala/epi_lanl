CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-11-24T10:16:16Z AOML 3.0 creation; 2016-08-07T21:36:41Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20151124101616  20160807143641  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               UA   AO  5286_8897_085                   2C  D   APEX                            6531                            072314                          846 @ׁ����1   @ׁ)��$@3E�����c I�^5?1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    UA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dky�Dk��Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy� D��D�L�D��fD�ٚD�	�D�33D���D���D���D�6fD�s3D�� D�fD�@ D�|�D�ɚD�3D�,�D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�G�@�{A
=A#
=AC
=Ac
=A��A��A��A��A��AхA�A�B BBBB B(B0B8B@BHBPBXB`BhBpBxB�aHB�aHB�aHB�aHB�aHB�aHB��{B���B�aHB�aHB�aHB�aHB�aHB�.B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC 0�C0�C0�C0�C0�C
0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C 0�C"0�C$0�C&0�C(0�C*0�C,0�C.0�C00�C20�C40�C60�C80�C:0�C<0�C>0�C@0�CB0�CD0�CF0�CH0�CJ0�CL0�CN0�CP0�CR0�CT0�CV0�CX0�CZ0�C\0�C^0�C`0�Cb0�Cd0�Cf0�Ch0�Cj0�Cl0�Cn0�Cp0�Cr0�Ct0�Cv0�Cx0�Cz0�C|0�C~0�C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�%C�RC�RC�RC�RC�RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!)D!�)D")D"�)D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3�D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk��Dl�Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dtl)Dy�)D��D�R�D��zD�߮D��D�9GD���D���D���D�<zD�yGD��D�zD�FDڂ�D�ϮD�GD�2�D�o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�  A��A��/AՁA�n�A�n�A�bNA�ZA�ZA�XA�VA�VA�Q�A�Q�A�Q�A�^5A�t�A�z�AՃAՉ7AՋDAՕ�Aա�Aգ�Aՙ�AՏ\AՍPA�~�A�|�AՉ7AՓuAՋDAՇ+AՅAՃAՃA�t�A�
=AӰ!AҋDA�\)A�=qA�=qA�VA�A�hsA�~�A�1'A��A�\)A���A�/A� �A�/A�ffA��jA�ĜA���A�l�A��A�XA�{A�E�A��A��A��A��mA�9XA���A��wA�bA���A�C�A�bA���A��A�Q�A���A��A�v�A�ĜA�XA�$�A�n�A��yA�v�A�v�A�G�A���A�ĜA��#A���A��/A��A�G�A��A��;A��A�oA�S�A���A�C�A�33A��A���A�O�A�
=A�ZA�{A�%A���A���A�JA|�Av��ApbNAkƨAgƨAbȴA_G�A\v�AZ�/AY/AX��AW��AWx�AU��ATȴARĜAPVAM��ALĜAK�AHAD^5A@��A?x�A=hsA;��A9\)A6M�A4��A3�A1��A/�A,�jA,��A-S�A+|�A+;dA+oA)t�A'��A'�hA';dA%��A#G�A"bA"ĜA"r�A"ffA!��A!/A �jA 5?A%A��AhsA�^A��A�yA^5A��A�A�A{A �A|�A;dA��A�wA�mAVA%A�A�^AQ�A��A?}A
=qA(�A�^AhsAA�A�A �/A��A�A?}A�jAƨ@�~�@�@���@�S�@�=q@�ƨ@�hs@�S�@�M�@�J@�{@�J@�O�@�"�@�`B@�@��@���@�~�@���@��@��@�z�@㝲@�(�@ܼj@��;@�$�@��H@�@ա�@Ձ@�`B@�n�@�-@ӕ�@�?}@Ͼw@�ff@�X@̼j@̣�@�+@ʧ�@ȴ9@ǝ�@�p�@�p�@���@ȃ@�V@ǶF@�p�@�=q@�&�@�b@�(�@�  @��@�\)@��@�@���@�z�@�Z@ÍP@�%@�5?@��`@��9@�X@�=q@���@ÍP@��H@��-@��h@��h@��@��@���@�ff@�$�@���@��@��T@���@��T@�5?@���@���@�ȴ@�=q@��7@�bN@��;@�\)@�"�@�@��7@�&�@��@�Ĝ@���@�`B@�/@�Ĝ@��w@���@�l�@��F@��u@��D@��@��D@�?}@���@���@��h@���@��R@�ȴ@�ȴ@���@��@�?}@�Z@��P@��
@���@�E�@�hs@���@���@���@�A�@���@�33@�S�@���@�A�@�S�@��y@�V@�5?@�@���@��\@�V@��@��T@��-@���@�@���@�t�@�+@�~�@���@��;@���@�9X@�Z@�Z@���@�bN@��j@��@���@��/@�j@�1@���@�K�@���@�\)@��@�J@�{@�^5@���@��F@�S�@�+@�;d@�"�@��m@�(�@�(�@�Q�@�\)@�33@��@��@�hs@�9X@��@��H@��H@�v�@��@��^@���@�1'@�9X@�Z@�9X@�I�@�j@��`@�Ĝ@��j@�Z@� �@���@���@�dZ@�+@�o@�ȴ@�M�@�@��h@�?}@�&�@���@�  @�
=@�K�@�+@�v�@�n�@�ff@��@�33@�33@�;d@�l�@���@�|�@�S�@�C�@��y@�ȴ@��+@�^5@�{@�@�J@���@�x�@�X@�7L@�`B@��#@���@�o@��@��@���@�E�@�G�@�/@�/@��@�Ĝ@�A�@�1@��@�  @�b@��@���@��;@��w@��P@�;d@��@���@�^5@�-@��@�-@��@�J@��@��@���@~v�@qX@h�u@`bN@V��@Pb@I&�@A%@;��@4��@/�P@*M�@#�m@5?@x�@V@��@�-@
-1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111A�  A��A��/AՁA�n�A�n�A�bNA�ZA�ZA�XA�VA�VA�Q�A�Q�A�Q�A�^5A�t�A�z�AՃAՉ7AՋDAՕ�Aա�Aգ�Aՙ�AՏ\AՍPA�~�A�|�AՉ7AՓuAՋDAՇ+AՅAՃAՃA�t�A�
=AӰ!AҋDA�\)A�=qA�=qA�VA�A�hsA�~�A�1'A��A�\)A���A�/A� �A�/A�ffA��jA�ĜA���A�l�A��A�XA�{A�E�A��A��A��A��mA�9XA���A��wA�bA���A�C�A�bA���A��A�Q�A���A��A�v�A�ĜA�XA�$�A�n�A��yA�v�A�v�A�G�A���A�ĜA��#A���A��/A��A�G�A��A��;A��A�oA�S�A���A�C�A�33A��A���A�O�A�
=A�ZA�{A�%A���A���A�JA|�Av��ApbNAkƨAgƨAbȴA_G�A\v�AZ�/AY/AX��AW��AWx�AU��ATȴARĜAPVAM��ALĜAK�AHAD^5A@��A?x�A=hsA;��A9\)A6M�A4��A3�A1��A/�A,�jA,��A-S�A+|�A+;dA+oA)t�A'��A'�hA';dA%��A#G�A"bA"ĜA"r�A"ffA!��A!/A �jA 5?A%A��AhsA�^A��A�yA^5A��A�A�A{A �A|�A;dA��A�wA�mAVA%A�A�^AQ�A��A?}A
=qA(�A�^AhsAA�A�A �/A��A�A?}A�jAƨ@�~�@�@���@�S�@�=q@�ƨ@�hs@�S�@�M�@�J@�{@�J@�O�@�"�@�`B@�@��@���@�~�@���@��@��@�z�@㝲@�(�@ܼj@��;@�$�@��H@�@ա�@Ձ@�`B@�n�@�-@ӕ�@�?}@Ͼw@�ff@�X@̼j@̣�@�+@ʧ�@ȴ9@ǝ�@�p�@�p�@���@ȃ@�V@ǶF@�p�@�=q@�&�@�b@�(�@�  @��@�\)@��@�@���@�z�@�Z@ÍP@�%@�5?@��`@��9@�X@�=q@���@ÍP@��H@��-@��h@��h@��@��@���@�ff@�$�@���@��@��T@���@��T@�5?@���@���@�ȴ@�=q@��7@�bN@��;@�\)@�"�@�@��7@�&�@��@�Ĝ@���@�`B@�/@�Ĝ@��w@���@�l�@��F@��u@��D@��@��D@�?}@���@���@��h@���@��R@�ȴ@�ȴ@���@��@�?}@�Z@��P@��
@���@�E�@�hs@���@���@���@�A�@���@�33@�S�@���@�A�@�S�@��y@�V@�5?@�@���@��\@�V@��@��T@��-@���@�@���@�t�@�+@�~�@���@��;@���@�9X@�Z@�Z@���@�bN@��j@��@���@��/@�j@�1@���@�K�@���@�\)@��@�J@�{@�^5@���@��F@�S�@�+@�;d@�"�@��m@�(�@�(�@�Q�@�\)@�33@��@��@�hs@�9X@��@��H@��H@�v�@��@��^@���@�1'@�9X@�Z@�9X@�I�@�j@��`@�Ĝ@��j@�Z@� �@���@���@�dZ@�+@�o@�ȴ@�M�@�@��h@�?}@�&�@���@�  @�
=@�K�@�+@�v�@�n�@�ff@��@�33@�33@�;d@�l�@���@�|�@�S�@�C�@��y@�ȴ@��+@�^5@�{@�@�J@���@�x�@�X@�7L@�`B@��#@���@�o@��@��@���@�E�@�G�@�/@�/@��@�Ĝ@�A�@�1@��@�  @�b@��@���@��;@��w@��P@�;d@��@���@�^5@�-@��@�-@��@�J@��G�O�@���@~v�@qX@h�u@`bN@V��@Pb@I&�@A%@;��@4��@/�P@*M�@#�m@5?@x�@V@��@�-@
-1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
dZB
dZB
`BB
XB
XB
YB
XB
YB
ZB
ZB
[#B
\)B
\)B
\)B
]/B
`BB
ffB
hsB
iyB
l�B
n�B
q�B
x�B
�B
�7B
�VB
��B
��B
��B
�!B
�dB
�jB
�jB
�jB
�qB
��B
ĜB
��BB.BbNB�1B��BuB/BF�BW
BVBjB|�B�B�hB��B�B�3B�^BƨB�#B�
BĜB�B�LBÖB��B��BÖB�9B�hBffBgmB~�B�Bo�Bp�B|�B�%B�DB�BW
B@�B@�B9XB,B%�B"�B�BhBB��B�B�B��By�Bo�BjBffBbNBbNBXBM�BE�B:^B'�B�BhBB
�B
�LB
y�B
F�B
+B
PB	�B	�B	�B	�PB	p�B	YB	>wB	)�B	�B	uB	JB	1B	B	B��B��B�B�fB�)B�
B��BÖB�XB�FB�9B�3B�3B�jB�}B�dB��B��B�9B��B�?B�}BȴB�B�B�
B�B�B�B��BȴBŢB�B�TB�B�B�sB�TB�5BÖB�dB��B��B��B��B�B�5B�5B�#B��B�NB�NB��B��B��B��B�B�B�`B�B��B��B��BǮB�wB�B��B��B�bB�VB��B��BŢB��B�?B��B�B�DB��B�^B��B��B��B��B��B��B�?B�LB��B��B�B��B�uB��B�'B�B��BŢB��B�?B�B��B��B��B��B��B��B��B�B�B��B��B��B�uB�uB��B��B�{B��B��B��B�9B�RB�^B�}BɺBȴB��B�XB�wB��B��B��B��B�B�B�B�B�B��B��B	DB	DB	1B	
=B	oB	�B	#�B	,B	.B	'�B	�B	DB		7B	1B	1B	1B		7B	
=B	JB	JB	JB	\B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	(�B	)�B	)�B	,B	1'B	49B	:^B	B�B	C�B	F�B	J�B	W
B	]/B	`BB	e`B	q�B	t�B	t�B	u�B	x�B	{�B	}�B	}�B	}�B	~�B	|�B	w�B	t�B	o�B	l�B	iyB	hsB	ffB	gmB	hsB	ffB	cTB	jB	q�B	s�B	u�B	w�B	{�B	�B	��B	��B	��B	��B	��B	��B	�!B	�XB	�XB	�RB	�RB	�9B	�B	�B	�B	�'B	�-B	�dB	�wB	�}B	B	ĜB	ĜB	ÖB	ÖB	ÖB	ƨB	ȴB	ƨB	ÖB	ŢB	ƨB	ÖB	��B	��B	��B	B	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	��B	��B	ɺB	ɺB	ɺB	ǮB	ƨB	ɺB	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�)B	�/B	�;B	�HB	�HB	�BB	�BB	�BB	�HB	�NB	�NB	�NB	�HB	�;B	�BB	�BB	�;B	�BB	�BB	�#B	�
B	�B	�B	�)B	�5B	�;B	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�ZB	�ZB	�`B	�sB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
\B
�B
$�B
,B
33B
8RB
=qB
C�B
I�B
N�B
T�B
YB
_;B
dZB
iyB
m�B
q�B
u�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111B
dOB
dOB
`8B
XB
XB
YB
XB
YB
ZB
ZB
[B
\B
\B
\B
]$B
`:B
f[B
hkB
ipB
l�B
n�B
q�B
x�B
��B
�,B
�JB
�vB
��B
��B
�B
�ZB
�_B
�]B
�[B
�fB
�wB
ĔB
��BB.Bb?B�"B�yBeB/BF�BV�BU�BjrB|�B��B�VB��B��B�#B�RBƗB�B��BČB�B�<BÄBʳB��BÄB�+B�TBfUBg[B~�B��Bo�Bp�B|�B�B�4B��BV�B@uB@rB9HB+�B%�B"�B�BYBB��B�zB�B��By�Bo�BjlBfTBb;Bb>BW�BM�BE�B:IB'�B�BYBB
�xB
�>B
y�B
F�B
*�B
CB	�B	��B	� B	�GB	p�B	YB	>qB	)�B	�B	rB	FB	,B	B	B��B��B�B�dB�(B�B��BÖB�WB�FB�9B�3B�5B�iB�{B�bB��B��B�7B��B�>B�yBȲB�B�B�B�B�B�B��BȰBŞB�B�QB�B�B�mB�PB�2BÐB�aB��B��B��B��B�B�2B�/B�B��B�JB�IB��B��B��B��B�B�B�XB�B��B��B��BǩB�tB�
B��B��B�`B�TB��B�~BŜB��B�:B��B�B�BB��B�ZB��B��B��B��B��B��B�9B�HB��B��B�	B��B�qB��B�#B�B�|BŜB�}B�9B��B��B��B��B��B��B��B��B��B��B��B��B��B�qB�pB�{B��B�vB��B��B��B�3B�IB�ZB�uBɵBȬB�|B�SB�oB��B��B��B��B�B�B�B�B�B��B��B	:B	;B	'B	
1B	cB	�B	#�B	+�B	.
B	'�B	�B	:B		/B	*B	(B	(B		/B	
0B	?B	@B	>B	SB	vB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	(�B	)�B	)�B	+�B	1B	4,B	:QB	B�B	C�B	F�B	J�B	V�B	]"B	`4B	eSB	q�B	t�B	t�B	u�B	x�B	{�B	}�B	}�B	}�B	~�B	|�B	w�B	t�B	o�B	l}B	ikB	hgB	fZB	gaB	hgB	fWB	cHB	jsB	q�B	s�B	u�B	w�B	{�B	�
B	��B	��B	��B	��B	��B	��B	�B	�IB	�IB	�BB	�AB	�(B	�B	��B	�B	�B	�B	�TB	�hB	�mB	�B	ċB	ČB	ÄB	ÆB	ÃB	ƙB	ȣB	ƗB	ÄB	ŒB	ƖB	ÄB	�yB	�{B	�xB	B	ĎB	˶B	��B	��B	��B	��B	��B	��B	��B	��B	̻B	ȟB	ʱB	ʰB	ɩB	ɩB	ɬB	ǜB	ƚB	ɩB	̼B	��B	��B	��B	�B	�B	�B	�B	�B	�B	� B	�)B	�8B	�7B	�/B	�1B	�/B	�7B	�<B	�:B	�<B	�6B	�+B	�/B	�2B	�)B	�/B	�0B	�B	��B	��B	�B	�B	�%B	�)B	�9B	�;B	�;B	�<B	�:B	�<B	�CB	�BB	�BB	�HB	�HB	�NB	�dB	�mB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
 �B
HB
nB
$�B
+�B
3#B
8?B
=_B
C�B
I�B
N�B
T�B
YB
_&B
dGB
icB
m~B
q�B
u�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.19 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436412016080714364120160807143641  AO  ARCAADJP                                                                    20151124101616    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151124101616  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151124101616  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143641  IP                  G�O�G�O�G�O�                