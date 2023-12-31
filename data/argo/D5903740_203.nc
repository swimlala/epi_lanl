CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-11-05T18:01:44Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20171105180144  20190604095308  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055                            2C  D   APEX                            5374                            041511                          846 @�3g�DY1   @�3ο�@:z^5?|��c1XbM�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDyx�D�RD�N�D�y�D��)D�{D�-qD��D��\D�D�H�D�[�D��HD��D�Q�Dڈ D���D���D�G\D�j�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�{@�{A
=A#
=AC
=Ac
=A��A��A��A��A��AхA�A�B BBBB B(B0B8B@BHBPBXB`BhBpBxB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC 0�C0�C0�C0�C0�C
0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C 0�C"0�C$0�C&0�C(0�C*0�C,0�C.0�C00�C20�C40�C60�C80�C:0�C<0�C>0�C@0�CB0�CD0�CF0�CH0�CJ0�CL0�CN0�CP0�CR0�CT0�CV0�CX0�CZ0�C\0�C^0�C`0�Cb0�Cd0�Cf0�Ch0�Cj0�Cl0�Cn0�Cp0�Cr0�Ct0�Cv0�Cx0�Cz0�C|0�C~0�C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D��D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!)D!�)D")D"�)D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dt�)DtҏDy�D�fD�T�D�� D��=D��D�3�D��3D��pD�3D�O
D�a�D��\D��D�X DڎD���D���D�MpD�p�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ƨA��HA��mA��yA��yA��A��A��A��A��A��yA��HA�r�A��`A�r�A�E�A�9XA��A�ĜA�ƨA�~�A��Aǲ-A�|�A�oA�?}A��Aũ�A�A�A�{A�ƨA�Q�A���A���A��wA���A�S�A��A�{A�(�A��wA���A��!A�r�A�ƨA���A�x�A��mA���A�  A���A��A��7A�|�A��uA�-A�A��\A��9A���A�t�A��A���A��#A��A�z�A�JA�ffA��#A��-A�=qA���A�jA���A��A�\)A��A�p�A���A�/A��`A�?}A��A���A�p�A�^5A�7LA��A���A��A���A�=qA�  A��A��+A�A%A~~�A}�A{��Ay�;Aw�Av��Au33Arn�Ao��Ak��Ai��AhffAd�RAc�PAa�mA_�-A\z�AW�#AWx�AV�AVbAS��AT{AU��AWx�AW�AV�\AU�AQ��AP��AP�AOVANr�AM�mALM�AK/AJ1'AIO�AHM�AF�\AD�yAC��AAA?��A@VAA�ABZAB�`AAhsA@I�A?�FA?G�A>��A>�+A>VA=�FA<I�A;?}A:��A9��A8��A7�
A7�A6�uA4��A3�;A2��A0�!A/��A.ĜA. �A-G�A,bNA+�7A*ZA)�-A(ĜA'�^A&�+A%�TA%�A$5?A"�A!\)A  �A�yAȴA  Az�A�PA��A�yA�A��A`BA��A�9AJA|�A�A�\A�A�AZA��AE�A�PAZA�FA��A;dA�9AƨAoA��A
�yA
5?A	x�A�jA1'A��A"�A��A�A��A��AO�A�uA��A ��@��;@��H@���@�%@��m@���@��P@���@�1'@��@�@�`B@�b@�C�@���@���@�dZ@�^@��;@��@�G�@�b@�E�@��/@���@�;d@��@�/@�j@��@�l�@�C�@�o@��@�5?@���@պ^@�z�@Ӯ@җ�@���@�Q�@�+@�v�@�p�@̴9@�S�@ɩ�@��`@���@�bN@�b@�  @��y@��#@���@�Z@���@���@�%@��;@��H@�$�@��#@��7@��/@���@��H@�~�@���@��@���@�9X@�C�@��H@��+@��@�"�@�J@��@�V@�Ĝ@���@��@�1'@���@���@��y@���@�/@��@���@���@�@�p�@�V@��D@� �@��@��w@���@�t�@�\)@�C�@��y@�5?@�7L@���@�@��H@��R@�ȴ@��!@�^5@��-@�I�@�dZ@�C�@�o@���@�M�@�$�@�{@�J@�@��@���@���@�?}@���@�Q�@���@���@���@�M�@�{@��@�@��@�A�@��@��F@�|�@�
=@�v�@�J@��^@��h@�hs@�7L@�V@��@�Q�@�b@���@���@��@���@�C�@��\@��-@�`B@�?}@�&�@��@���@�A�@� �@�b@�C�@��@�-@���@�G�@���@�Ĝ@��u@�9X@�ƨ@��P@�\)@���@��!@�n�@�V@�5?@�$�@���@���@���@�@���@��h@�p�@���@���@��9@���@�I�@~�+@~$�@}��@}`B@}`B@}O�@}?}@}/@|�j@|I�@{��@{ƨ@{��@{��@|(�@|9X@|(�@{�@{@z��@{��@z��@z�!@z�H@z�@z�H@z��@z��@z��@z��@z�!@z��@z~�@y��@y��@y7L@x��@x��@x1'@wl�@v��@vv�@vV@u�@u@u�@t�@tZ@t1@s��@s�
@s�@sdZ@s33@r^5@r-@q��@q��@q&�@pĜ@p��@p��@pQ�@n��@eN<@^�s@Y�S@U�@P9X@K i@D�z@>�'@6_�@0��@,�$@$�[@~�@!�@@:*@�6@
��@qv@<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�ƨA��HA��mA��yA��yA��A��A��A��A��A��yA��HA�r�A��`A�r�A�E�A�9XA��A�ĜA�ƨA�~�A��Aǲ-A�|�A�oA�?}A��Aũ�A�A�A�{A�ƨA�Q�A���A���A��wA���A�S�A��A�{A�(�A��wA���A��!A�r�A�ƨA���A�x�A��mA���A�  A���A��A��7A�|�A��uA�-A�A��\A��9A���A�t�A��A���A��#A��A�z�A�JA�ffA��#A��-A�=qA���A�jA���A��A�\)A��A�p�A���A�/A��`A�?}A��A���A�p�A�^5A�7LA��A���A��A���A�=qA�  A��A��+A�A%A~~�A}�A{��Ay�;Aw�Av��Au33Arn�Ao��Ak��Ai��AhffAd�RAc�PAa�mA_�-A\z�AW�#AWx�AV�AVbAS��AT{AU��AWx�AW�AV�\AU�AQ��AP��AP�AOVANr�AM�mALM�AK/AJ1'AIO�AHM�AF�\AD�yAC��AAA?��A@VAA�ABZAB�`AAhsA@I�A?�FA?G�A>��A>�+A>VA=�FA<I�A;?}A:��A9��A8��A7�
A7�A6�uA4��A3�;A2��A0�!A/��A.ĜA. �A-G�A,bNA+�7A*ZA)�-A(ĜA'�^A&�+A%�TA%�A$5?A"�A!\)A  �A�yAȴA  Az�A�PA��A�yA�A��A`BA��A�9AJA|�A�A�\A�A�AZA��AE�A�PAZA�FA��A;dA�9AƨAoA��A
�yA
5?A	x�A�jA1'A��A"�A��A�A��A��AO�A�uA��A ��@��;@��H@���@�%@��m@���@��P@���@�1'@��@�@�`B@�b@�C�@���@���@�dZ@�^@��;@��@�G�@�b@�E�@��/@���@�;d@��@�/@�j@��@�l�@�C�@�o@��@�5?@���@պ^@�z�@Ӯ@җ�@���@�Q�@�+@�v�@�p�@̴9@�S�@ɩ�@��`@���@�bN@�b@�  @��y@��#@���@�Z@���@���@�%@��;@��H@�$�@��#@��7@��/@���@��H@�~�@���@��@���@�9X@�C�@��H@��+@��@�"�@�J@��@�V@�Ĝ@���@��@�1'@���@���@��y@���@�/@��@���@���@�@�p�@�V@��D@� �@��@��w@���@�t�@�\)@�C�@��y@�5?@�7L@���@�@��H@��R@�ȴ@��!@�^5@��-@�I�@�dZ@�C�@�o@���@�M�@�$�@�{@�J@�@��@���@���@�?}@���@�Q�@���@���@���@�M�@�{@��@�@��@�A�@��@��F@�|�@�
=@�v�@�J@��^@��h@�hs@�7L@�V@��@�Q�@�b@���@���@��@���@�C�@��\@��-@�`B@�?}@�&�@��@���@�A�@� �@�b@�C�@��@�-@���@�G�@���@�Ĝ@��u@�9X@�ƨ@��P@�\)@���@��!@�n�@�V@�5?@�$�@���@���@���@�@���@��h@�p�@���@���@��9@���@�I�@~�+@~$�@}��@}`B@}`B@}O�@}?}@}/@|�j@|I�@{��@{ƨ@{��@{��@|(�@|9X@|(�@{�@{@z��@{��@z��@z�!@z�H@z�@z�H@z��@z��@z��@z��@z�!@z��@z~�@y��@y��@y7L@x��@x��@x1'@wl�@v��@vv�@vV@u�@u@u�@t�@tZ@t1@s��@s�
@s�@sdZ@s33@r^5@r-@q��@q��@q&�@pĜ@p��@p��G�O�@n��@eN<@^�s@Y�S@U�@P9X@K i@D�z@>�'@6_�@0��@,�$@$�[@~�@!�@@:*@�6@
��@qv@<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B1BhBhB	7B	7BB%B%�B-B,B,B0!B9XBA�BD�BM�BL�BK�BM�BS�B�BɺB��B�oB�JB�VB��B�uBs�BaHB[#BP�BR�BZBP�BL�BH�BD�B;dB'�B�B\BB��B�mB�HB�B��BĜB�FB��B�hB�%Bz�Bt�Bm�BgmBffBiyBcTB_;BS�BF�B1'B&�BuB+B  B
��B
�B
�)B
ÖB
�wB
�wB
�qB
�dB
�XB
�3B
�B
��B
��B
�B
�B
��B
��B
��B
�VB
�B
u�B
XB
I�B
9XB
�B	��B	��B	�!B	��B	�B	r�B	bNB	H�B	+B��B��B�B�B�yB�B	�B	dZB	cTB	aHB	XB	?}B	6FB	5?B	2-B	-B	(�B	�B	bB	1B	B��B�B�;B��BǮB�wB��B�B	B	 �B	�B	�B	oB	\B	PB	DB	
=B	+B	B��B��B�B�B�B�yB�ZB�B��BĜB�XB�?B�!B�B�B��B��B��B�{B�\B�=B�B�7B�DB�+B}�Bt�Bp�Bm�Bm�Bk�Be`B`BB]/B]/B_;B`BB\)BZBZB[#BYBT�BR�BO�BK�BH�BE�BE�BD�BD�BC�BC�BB�BE�BD�BB�B>wB=qB<jB;dB:^B8RB7LB7LB5?B33B1'B0!B.B,B)�B(�B'�B&�B&�B%�B!�B�B�B#�B�B �B!�B"�B#�B$�B$�B#�B!�B"�B#�B"�B �B�B�B!�B!�B �B �B �B �B �B%�B(�B(�B)�B)�B&�B&�B&�B'�B'�B&�B'�B&�B%�B$�B!�B�B�B�BuBoB{B{BhBbBhBhBbBbB{B�B�B�B�B�B �B!�B#�B#�B"�B(�B-B/B33B5?B49B5?B8RB:^B=qB>wB?}B?}B?}BC�BH�BH�BJ�BO�BO�BO�BS�BVBZBe`BgmBhsBk�Bn�Bp�Bp�Bq�Bq�Br�Bt�Bz�Bz�Bz�Bz�Bz�B~�B�B�B�B�B�B�B�B�%B�=B�PB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�-B�?B�LB�jB�}B�}B�}B��BBÖBĜBŢBɺB��B��B��B��B��B��B�B�/B�NB�`B�fB�mB�yB�B�B�B�B�B��B��B	  B	B	B	%B	+B		7B	PB	\B	bB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	%�B	&�B	,B	.B	.B	/B	1'B	;dB	=qB	@�B	B�B	C�B	D�B	F�B	G�B	H�B	H�B	J�B	O�B	S�B	W
B	ZB	\)B	\)B	]/B	_;B	bNB	gmB	jB	k�B	n�B	o�B	q�B	q�B	r�B	r�B	s�B	t�B	t�B	u�B	x�B	z�B	~�B	�B	�B	�B	�+B	�=B	�DB	�JB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	��B	�B	��B
 �B
B
#nB
,=B
2�B
5�B
8�B
C�B
H�B
M�B
T,B
\B
a�B
iyB
o�B
utB
x�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B!BXBXB	'B	'B�BB%�B- B+�B+�B0B9IBAzBD�BM�BL�BK�BM�BS�B�BɩB��B�`B�=B�HB�oB�bBs�Ba4B[BP�BR�BZBP�BL�BH�BD�B;PB'�B�BLBB��B�_B�9B��B̽BĆB�3B��B�TB�Bz�Bt�BmBgZBfSBigBc@B_'BS�BF�B1B&�BdBB
��B
��B
�B
�B
ÄB
�dB
�gB
�]B
�TB
�CB
�B
�B
��B
��B
��B
��B
��B
��B
�tB
�BB
��B
u�B
W�B
I�B
9EB
B	��B	ͿB	�B	��B	�B	r�B	b;B	H�B	*�B��B��B�B�wB�dB�B	�B	dFB	cAB	a3B	W�B	?gB	61B	5*B	2B	,�B	(�B	�B	MB	B	B��B��B�&B��BǗB�dB��B�B	B	 �B	�B	lB	WB	FB	;B	/B	
'B	B	 �B��B��B�B�{B�tB�cB�DB��BʬBĈB�AB�)B�B�B��B��B��B��B�cB�CB�'B�
B�#B�1B�B}�Bt�Bp�Bm{Bm|BkoBeJB`.B]B]B_#B`-B\BZBZB[BYBT�BR�BO�BK�BH�BE�BE�BD�BD�BC�BCBBzBE�BD�BBwB>^B=YB<TB;NB:GB8;B74B75B5*B3B1B0
B-�B+�B)�B(�B'�B&�B&�B%�B!�B�B�B#�B�B �B!�B"�B#�B$�B$�B#�B!�B"�B#�B"�B �B�B�B!�B!�B �B �B �B �B �B%�B(�B(�B)�B)�B&�B&�B&�B'�B'�B&�B'�B&�B%�B$�B!�B�B�BvB^BVBcBcBOBHBOBOBGBIBcB�B�B�B�B�B �B!�B#�B#�B"�B(�B,�B/B3B5'B4"B5'B8:B:FB=YB>_B?fB?cB?dBC}BH�BH�BJ�BO�BO�BO�BS�BU�BZBeJBgTBhZBknBn�Bp�Bp�Bq�Bq�Br�Bt�Bz�Bz�Bz�Bz�Bz�B~�B��B��B��B��B��B� B�B�B�$B�8B�VB�]B�aB�tB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�%B�2B�SB�eB�dB�eB�jB�uB�}BąBňBɣB͹B��B��B��B��B��B��B�B�7B�DB�NB�TB�]B�jB�~B�B��B�B��B��B��B	�B	�B	B	B		B	7B	AB	GB	`B	uB	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	%�B	&�B	+�B	-�B	-�B	/B	1B	;LB	=ZB	@hB	BvB	C~B	D�B	F�B	G�B	H�B	H�B	J�B	O�B	S�B	V�B	ZB	\B	\B	]B	_#B	b2B	gUB	jgB	koB	n~B	o�B	q�B	q�B	r�B	r�B	s�B	t�B	t�B	u�B	x�B	z�B	~�B	��B	��B	��B	�B	�&B	�)B	�0B	�BB	�GB	�\B	�nB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B	� B	�B
 �B
�B
#UB
,&B
2�B
5�B
8�B
C�B
H�B
M�B
TB
[�B
a�B
i`B
o�B
uZB
x�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.19 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201906040953082019060409530820190604095308  AO  ARCAADJP                                                                    20171105180144    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171105180144  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171105180144  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604095308  IP                  G�O�G�O�G�O�                