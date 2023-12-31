CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-08-21T19:17:28Z AOML 3.0 creation; 2016-08-07T21:36:38Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150821191728  20160807143638  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               CA   AO  5286_8897_067                   2C  D   APEX                            6531                            072314                          846 @�i[��C1   @�i[��=.@3�`A�7L�cJ�x���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    CA   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy�fD�fD�6fD�i�D���D��D�C3D���D��fD�	�D�C3D���Dǹ�D�#3D�FfD�\�D��3D�� D�6fD�y�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @E@�{@�{A
=A#
=AC
=Ac
=A��A��A��A��A��AхA�A�B B	(�BBB B(B0B8B@BHBPBXB`BhBpBxB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHBؔ{Bܔ{B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC 0�C0�C0�C0�C0�C
0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C 0�C"0�C$0�C&0�C(0�C*0�C,0�C.0�C00�C20�C40�C60�C80�C:0�C<0�C>0�C@0�CB0�CD0�CF0�CH0�CJ0�CL0�CN0�CP0�CR0�CT0�CV0�CXJ>CZ0�C\0�C^0�C`0�Cb0�Cd0�Cf0�Ch0�Cj0�Cl0�Cn0�Cp0�Cr0�Ct0�Cv0�Cx0�Cz0�C|0�C~0�C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�%C�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!)D!�)D")D"�)D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dt�)Dy��D�zD�<zD�o�D���D��D�IGD���D��zD��D�IGD���Dǿ�D�)GD�LzD�b�D��GD��D�<zD��D��z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A���A��/A��/A���A���A���A�A޺^AޓuA�l�A�
=Aݡ�A�1A�AܓuA�v�A�S�A�ZA�G�A�+A� �A�?}A�ĜA͝�Aͩ�A̶FA��TA�p�A�
=Aʉ7A�1AɃAȰ!A�`BA�7LAš�A���AÏ\A�ȴA��A�C�A�z�A���A�A�A�dZA���A�7LA�G�A�7LA��`A��A�l�A�S�A�E�A�K�A���A�^5A��A�VA�O�A��+A��/A���A���A� �A�  A���A���A���A�"�A�bA�jA��A��7A��uA�ZA� �A��A��A�bNA�+A��A��7A�/A�VA��A�l�A�{A�(�A���A� �A�ZA�A�\)A�x�A��FA���A��+A�XA��DA�C�A�A���A�|�A�^5A�VA�?}A�1'A}�Ay|�Au��Ar�`Ao
=Ak"�Ah��Af�DAd��AbE�Aa�
AahsA^ĜAZE�AYS�AXjAV  AR1AN^5AK��AJ�RAI7LAFȴADn�AB�HA@�!A>��A=�A;G�A9\)A7|�A6ZA61A5�A3A2VA1�-A1G�A1A0$�A.JA-\)A-oA,  A*VA)?}A(r�A'�A'�7A&��A%��A%\)A$��A$�A#ƨA#�A"JA �yAffAt�A&�A�RA=qA��A^5A��A�/AA|�Ar�AƨA��A&�A1'A�AXA�HA�A�HAz�At�AoAȴAz�AA"�A
�jA	�A�!AK�Av�A�
A\)AĜA1'A�A��A;dAAVA ��A �!A ��A Z@��@�G�@��@��-@�7L@�ȴ@�V@��j@�1'@��@���@�n�@��@�^@��m@�{@�I�@�ȴ@띲@��
@���@��y@�A�@�P@��H@���@�v�@���@�`B@�D@�  @⟾@���@��D@ߕ�@���@ޏ\@�`B@��`@ܴ9@��/@ܴ9@ܴ9@�Z@��;@�S�@�+@�
=@��@�E�@��#@ف@�O�@�&�@�Ĝ@�r�@�b@�S�@��@�~�@��@թ�@��@��@�V@���@���@Гu@ѩ�@ѩ�@���@���@�C�@�;d@�(�@�|�@Η�@���@��@���@�I�@�r�@ź^@�z�@�|�@ǥ�@�|�@���@�1@���@�t�@�(�@��@�r�@�S�@š�@���@Å@�n�@���@��T@���@�hs@���@�z�@�-@�b@��h@��D@�1@�~�@�-@��T@�O�@�Ĝ@�Q�@�(�@��m@��9@�33@��H@���@���@��9@��9@�1'@�1@� �@�1@��P@��@�&�@��`@�j@���@��@�"�@�S�@�o@��@�M�@���@���@��R@�^5@���@��
@��m@��@�o@�7L@�|�@�b@��D@�C�@���@�ff@��@���@��-@��^@���@�E�@�C�@�C�@�E�@�{@���@��@���@�I�@�33@��R@��!@��@�5?@���@���@�X@�O�@���@���@�j@�(�@�  @��@��@�ȴ@���@��+@�v�@�v�@�-@��T@�/@�r�@�(�@��w@���@�33@�@�ȴ@��R@��R@�n�@�5?@�-@�J@���@�ȴ@��@�1'@��@��m@�1@��@��;@�ƨ@�;d@��y@��@�n�@��T@���@�n�@�=q@���@��h@�O�@��u@�Q�@�A�@�1'@�  @�ƨ@�A�@�z�@��@��@���@�r�@��;@�"�@�K�@���@���@���@��F@�|�@��@�S�@�o@���@�$�@��T@��T@�p�@�?}@��/@��@�bN@�(�@��m@��@� �@�9X@�1'@� �@��@��@� �@�b@���@��
@�ƨ@��@�5?@��-@u��@p�@i�@d�j@`A�@V��@R��@JJ@B��@>$�@5�h@.$�@'�@!�@�@��@/@X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�A�A���A��/A��/A���A���A���A�A޺^AޓuA�l�A�
=Aݡ�A�1A�AܓuA�v�A�S�A�ZA�G�A�+A� �A�?}A�ĜA͝�Aͩ�A̶FA��TA�p�A�
=Aʉ7A�1AɃAȰ!A�`BA�7LAš�A���AÏ\A�ȴA��A�C�A�z�A���A�A�A�dZA���A�7LA�G�A�7LA��`A��A�l�A�S�A�E�A�K�A���A�^5A��A�VA�O�A��+A��/A���A���A� �A�  A���A���A���A�"�A�bA�jA��A��7A��uA�ZA� �A��A��A�bNA�+A��A��7A�/A�VA��A�l�A�{A�(�A���A� �A�ZA�A�\)A�x�A��FA���A��+A�XA��DA�C�A�A���A�|�A�^5A�VA�?}A�1'A}�Ay|�Au��Ar�`Ao
=Ak"�Ah��Af�DAd��AbE�Aa�
AahsA^ĜAZE�AYS�AXjAV  AR1AN^5AK��AJ�RAI7LAFȴADn�AB�HA@�!A>��A=�A;G�A9\)A7|�A6ZA61A5�A3A2VA1�-A1G�A1A0$�A.JA-\)A-oA,  A*VA)?}A(r�A'�A'�7A&��A%��A%\)A$��A$�A#ƨA#�A"JA �yAffAt�A&�A�RA=qA��A^5A��A�/AA|�Ar�AƨA��A&�A1'A�AXA�HA�A�HAz�At�AoAȴAz�AA"�A
�jA	�A�!AK�Av�A�
A\)AĜA1'A�A��A;dAAVA ��A �!A ��A Z@��@�G�@��@��-@�7L@�ȴ@�V@��j@�1'@��@���@�n�@��@�^@��m@�{@�I�@�ȴ@띲@��
@���@��y@�A�@�P@��H@���@�v�@���@�`B@�D@�  @⟾@���@��D@ߕ�@���@ޏ\@�`B@��`@ܴ9@��/@ܴ9@ܴ9@�Z@��;@�S�@�+@�
=@��@�E�@��#@ف@�O�@�&�@�Ĝ@�r�@�b@�S�@��@�~�@��@թ�@��@��@�V@���@���@Гu@ѩ�@ѩ�@���@���@�C�@�;d@�(�@�|�@Η�@���@��@���@�I�@�r�@ź^@�z�@�|�@ǥ�@�|�@���@�1@���@�t�@�(�@��@�r�@�S�@š�@���@Å@�n�@���@��T@���@�hs@���@�z�@�-@�b@��h@��D@�1@�~�@�-@��T@�O�@�Ĝ@�Q�@�(�@��m@��9@�33@��H@���@���@��9@��9@�1'@�1@� �@�1@��P@��@�&�@��`@�j@���@��@�"�@�S�@�o@��@�M�@���@���@��R@�^5@���@��
@��m@��@�o@�7L@�|�@�b@��D@�C�@���@�ff@��@���@��-@��^@���@�E�@�C�@�C�@�E�@�{@���@��@���@�I�@�33@��R@��!@��@�5?@���@���@�X@�O�@���@���@�j@�(�@�  @��@��@�ȴ@���@��+@�v�@�v�@�-@��T@�/@�r�@�(�@��w@���@�33@�@�ȴ@��R@��R@�n�@�5?@�-@�J@���@�ȴ@��@�1'@��@��m@�1@��@��;@�ƨ@�;d@��y@��@�n�@��T@���@�n�@�=q@���@��h@�O�@��u@�Q�@�A�@�1'@�  @�ƨ@�A�@�z�@��@��@���@�r�@��;@�"�@�K�@���@���@���@��F@�|�@��@�S�@�o@���@�$�@��T@��T@�p�@�?}@��/@��@�bN@�(�@��m@��@� �@�9X@�1'@� �@��@��@� �@�b@���@��
G�O�@��@�5?@��-@u��@p�@i�@d�j@`A�@V��@R��@JJ@B��@>$�@5�h@.$�@'�@!�@�@��@/@X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
�B
�B
�B
�B
�B
�'B
�-B
�FB
�XB
��B
ŢB
�qB
�XB
�XB
�RB
�jB
ĜB
ĜB
ŢB
ĜB
�-B
��BhsB�NB{B+B5?B>wBE�BC�BA�BS�BP�Bo�Bq�Bp�BaHBZBT�B]/BVBVBaHBv�B� B�B�+By�B�B�bB��B��B�B�dB��B�BB�B�B�B�B��B��B��B�{B�\B�Bq�B�1B�jB�'B��B��B�B��B�PB}�BiyBe`B^5BVB1'B�B�B+B�B�5B��BĜB�}B�dB��Bo�BYBYBT�BD�B0!B
=B
�B
�
B
��B
��B
�B
��B
l�B
J�B
%�B
1B	�sB	��B	�jB	��B	�B	r�B	e`B	\)B	M�B	I�B	C�B	2-B	�B	bB	1B��B�mB�B��B��BŢB�}B�XB�FB�-B�'B�B�B�3B�RB�XB�RB�LB�^B�jB�qB�wB�qB�jBŢB��B��B��B��B��B�B�B�B�)B�NB�ZB�`B�`B�TB�TB�BB�B�B�)B�/B�HB�;B�/B�ZB�fB�ZB�HB�HB�ZB�NB�5B�#B�B�B�B�#B�#B�#B�/B�#B�B�B�B�HB�BB�HB�/B�/B�5B�;B�;B�;B�;B�BB�HB�BB�;B�NB�fB�B��B��B��B��B	%B	1B	uB	�B	oB	oB	�B	�B	�B	 �B	!�B	�B	�B	�B	�B	bB	VB	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	%�B	$�B	$�B	(�B	.B	33B	49B	49B	5?B	5?B	5?B	6FB	9XB	<jB	=qB	=qB	=qB	<jB	;dB	=qB	=qB	<jB	>wB	@�B	G�B	J�B	L�B	Q�B	W
B	XB	ZB	YB	\)B	\)B	ZB	VB	Q�B	N�B	L�B	L�B	O�B	[#B	^5B	`BB	]/B	YB	`BB	hsB	iyB	ffB	aHB	VB	O�B	J�B	M�B	VB	e`B	u�B	e`B	R�B	O�B	YB	dZB	p�B	z�B	�B	�B	�B	�B	}�B	z�B	x�B	|�B	v�B	q�B	x�B	{�B	y�B	�1B	�B	|�B	x�B	v�B	r�B	r�B	q�B	r�B	r�B	s�B	t�B	v�B	~�B	z�B	y�B	t�B	s�B	r�B	v�B	x�B	{�B	}�B	�B	�B	|�B	{�B	z�B	y�B	x�B	z�B	}�B	�B	�B	�B	�B	�B	�=B	�uB	��B	��B	��B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�'B	�RB	�}B	ƨB	ɺB	ƨB	ƨB	ĜB	B	��B	��B	�qB	�wB	B	ÖB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�;B	�NB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�mB	�B	�B	�B	�B	�B	�sB	�BB	�#B	�B	�B	�)B	�5B	�5B	�;B	�BB	�BB	�;B	�;B	�5B	�/B	�BB	�ZB	�ZB	�TB	�NB	�TB	�NB	�NB	�NB	�NB	�NB	�NB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
+B
hB
�B
{B
�B
�B
 �B
-B
2-B
<jB
B�B
I�B
K�B
Q�B
VB
\)B
cTB
ffB
jB
m�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
��B
��B
��B
��B
��B
��B
� B
�
B
�B
�"B
�;B
�LB
�B
ŘB
�dB
�NB
�NB
�GB
�`B
ĐB
ĐB
ŗB
ēB
�"B
�}BhfB�?BkB*�B51B>hBE�BC�BA|BS�BP�Bo�Bq�Bp�Ba6BZBT�B]BU�BU�Ba8Bv�B�B�B�By�B�B�QB��B��B��B�TB��B�5B�BnB�B�B�}B��B��B�gB�KB�Bq�B�!B�[B�B��B��B� B��B�@B}�BicBeKB^BU�B1B�B�BB�uB�$B��BČB�lB�QB��Bo�BYBYBT�BD�B0B
,B
�oB
��B
��B
ʱB
�B
�vB
l~B
J�B
%�B
$B	�hB	��B	�_B	��B	�B	r�B	eXB	\"B	M�B	I�B	C�B	2'B	�B	^B	-B��B�kB�B��B��BŠB�|B�WB�HB�/B�&B�B�B�2B�QB�WB�QB�KB�]B�iB�nB�uB�pB�hBŠB��BʾB��B��B��B�B�B�B�&B�JB�VB�[B�]B�OB�OB�@B�B�B�&B�,B�BB�6B�+B�TB�aB�VB�BB�BB�UB�JB�1B�B�B�B�B�B�B�B�(B�B�B�B�B�CB�<B�BB�)B�+B�2B�7B�6B�6B�5B�;B�@B�;B�4B�JB�aB�B��B��B��B��B	B	&B	lB	�B	eB	fB	�B	�B	�B	 �B	!�B	�B	�B	yB	vB	YB	MB	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	%�B	$�B	$�B	(�B	.
B	3*B	4.B	4/B	51B	54B	55B	6<B	9KB	<^B	=hB	=fB	=fB	<aB	;[B	=eB	=dB	<^B	>lB	@xB	G�B	J�B	L�B	Q�B	V�B	XB	ZB	YB	\B	\B	ZB	U�B	Q�B	N�B	L�B	L�B	O�B	[B	^)B	`6B	]#B	YB	`5B	hgB	imB	f[B	a:B	U�B	O�B	J�B	M�B	U�B	eRB	u�B	eRB	R�B	O�B	YB	dOB	p�B	z�B	�B	�	B	�B	��B	}�B	z�B	x�B	|�B	v�B	q�B	x�B	{�B	y�B	�%B	�B	|�B	x�B	v�B	r�B	r�B	q�B	r�B	r�B	s�B	t�B	v�B	~�B	z�B	y�B	t�B	s�B	r�B	v�B	x�B	{�B	}�B	��B	�B	|�B	{�B	z�B	y�B	x�B	z�B	}�B	��B	��B	��B	��B	�B	�/B	�gB	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�AB	�mB	ƘB	ɩB	ƗB	ƗB	ĊB	�B	�zB	�sB	�aB	�fB	�B	ÄB	ǠB	ȡB	ɪB	ʲB	˵B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�=B	�=B	�CB	�CB	�JB	�HB	�HB	�]B	�uB	�yB	�B	�B	�xB	�cB	�0B	�B	�B	�B	�B	�%B	�$B	�*B	�0B	�3B	�)B	�*B	�&B	�B	�/B	�KB	�IB	�CB	�<B	�@B	�9B	�=B	�9B	�;B	�<B	�=B	�]B	�lB	�B	�B	�B	�B	�B	�uB	�~B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
 B
B
G�O�B
B
UB
�B
hB
sB
�B
 �B
,�B
2B
<UB
B~B
I�B
K�B
Q�B
U�B
\B
c>B
fTB
jkB
m|B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.19 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436382016080714363820160807143638  AO  ARCAADJP                                                                    20150821191728    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150821191728  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150821191728  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143638  IP                  G�O�G�O�G�O�                