CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-03-30T02:16:05Z AOML 3.0 creation; 2016-08-07T21:36:45Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160330021605  20160807143645  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               mA   AO  5286_8897_109                   2C  D   APEX                            6531                            072314                          846 @נ���f�1   @נ�Q��@3�E����c[n��P1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    mA   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DIy�DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDyS3D��D�C3D�l�D��3D�3D�<�D��3D���D�  D�P D�|�D��fD� D�I�D�y�D��3D�3D�9�D� D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@�{A
=A#
=AC
=Ac
=A��A��A��A��A��AхA�A�B BBBB B(B0\)B8B@BHBPBXB`BhBpBxB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB��{B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�ǮB�.B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC 0�C0�C0�C0�C0�C
0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C 0�C"0�C$0�C&0�C(0�C*0�C,0�C.0�C00�C20�C40�C60�C80�C:0�C<0�C>0�C@0�CB0�CD0�CF0�CH0�CJ0�CL0�CN0�CP0�CR0�CT0�CV0�CX0�CZ0�C\0�C^0�C`0�Cb0�Cd0�Cf0�Ch0�Cj0�Cl0�Cn0�Cp0�Cr0�Ct0�Cv0�Cx0�Cz0�C|0�C~0�C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC��C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!)D!�)D")D"�)D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI��DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dtr�Dy_\D��D�IGD�r�D��GD�GD�B�D��GD���D�D�VD���D��zD�D�O�D��D��GD�GD�?�D�D�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AˁA�~�A�ffA�ZA�S�A�I�A�+A�oA�VA���A��yA��#A���Aʩ�Aʇ+A�S�A�K�A�?}A�-A��AɮAə�AɅA�r�A�`BA�K�A�5?A� �A�1A���A��#AȶFAș�AȓuAȟ�Aȝ�Aȧ�A���A��AȼjAȬAȣ�Aȉ7A�r�A�K�A���A���A�|�A��A�ffA�M�A��HA�`BA���A�|�A�A�A��A�v�A�;dA���A�JA��A���A�hsA���A�?}A���A���A�ffA�(�A��A�"�A�O�A�+A��A��HA���A�A���A��PA�M�A��A��+A��A���A���A���A��TA�/A��+A��^A�&�A�A�A��^A�-A���A�r�A�33A�"�A� �A��`A���A��A��+A��^A��uA��!A�ȴA��^A�l�A�r�A�{A���A�A{|�Aw�Au&�Apn�Am%Ag;dAb5?A^^5A\bAW�AQ�AM�AI?}AHffAG33AE�;AEO�AD��ADZAB�jA@��A@�A?C�A>  A<bA:ȴA8��A6�jA5l�A5+A5
=A4ĜA4��A3x�A1��A01A/?}A-�A*ĜA)�TA)K�A)/A(=qA'S�A&��A%G�A#�7A!dZAr�A�7A33A��A�HA9XA�A�A�
AC�AbA33AA�A��A~�AE�A�A��AdZAƨAhsA+A{AAA
M�A	oA	&�A	��A
�/A�A/A
�\A
VA	G�AM�A%AS�A��A�9AbNA&�A 5?@��/@�`B@�-@��@�v�@���@�7L@��@�Ĝ@�9X@�~�@�hs@�l�@�@�@ꟾ@�P@�ff@��#@�(�@���@��@���@�X@�^5@�@��@��#@�I�@ݩ�@�G�@�7L@��@���@ܣ�@�ƨ@�&�@�bN@� �@�1@��
@���@�~�@�X@Ӆ@���@�=q@�p�@Л�@�A�@ύP@�v�@Ο�@Χ�@�^5@̴9@̃@ˍP@�"�@�"�@��y@�V@ɩ�@ȣ�@�S�@�-@�p�@��@�ff@���@�?}@���@�j@�r�@��@��D@���@� �@�C�@�o@�"�@�l�@�S�@�~�@�`B@�V@�1'@���@��;@�9X@��!@�=q@�n�@���@�7L@�&�@��@��@�Q�@�1@��
@���@�\)@��@���@�C�@�\)@�K�@�ȴ@�^5@��@��-@�X@�V@���@�(�@�|�@���@�V@�=q@��-@�`B@��h@�hs@��@���@�ƨ@�K�@��@���@�~�@�ff@�^5@�@��@�x�@���@���@��u@�1'@���@�33@�ȴ@�=q@��#@���@�7L@���@�r�@�Q�@��@�dZ@��@�V@��7@��@��h@���@��^@���@��-@��@�7L@��@�1'@��
@�o@���@��R@�V@���@��^@���@�p�@�O�@�G�@�X@�&�@���@���@�j@��@�|�@�33@��@���@�~�@�=q@���@�Ĝ@��@�A�@�9X@�Ĝ@��`@��/@��@��@�ȴ@�v�@�=q@�X@�x�@�7L@���@���@���@�r�@�Q�@�Z@��j@���@��@�1'@�1@��@��m@��m@��m@��
@�ƨ@�t�@���@�{@�{@��@�{@�%@���@�Z@� �@���@�ƨ@��F@���@�t�@�S�@��@���@�ff@�-@��-@��h@��7@��7@��@�7L@�%@��j@�r�@�9X@��;@���@�dZ@�;d@��@�-@��#@�hs@���@��9@�j@�Z@�A�@�1@���@�\)@�33@�
=@���@�~�@�=q@�J@���@��-@���@��@�x�@�x�@�hs@�%@��@���@���@x��@pĜ@i�@`A�@Y&�@P  @G;d@A%@9%@1G�@,1@%��@ �u@9X@��@��@��@
~�@K�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   AˁA�~�A�ffA�ZA�S�A�I�A�+A�oA�VA���A��yA��#A���Aʩ�Aʇ+A�S�A�K�A�?}A�-A��AɮAə�AɅA�r�A�`BA�K�A�5?A� �A�1A���A��#AȶFAș�AȓuAȟ�Aȝ�Aȧ�A���A��AȼjAȬAȣ�Aȉ7A�r�A�K�A���A���A�|�A��A�ffA�M�A��HA�`BA���A�|�A�A�A��A�v�A�;dA���A�JA��A���A�hsA���A�?}A���A���A�ffA�(�A��A�"�A�O�A�+A��A��HA���A�A���A��PA�M�A��A��+A��A���A���A���A��TA�/A��+A��^A�&�A�A�A��^A�-A���A�r�A�33A�"�A� �A��`A���A��A��+A��^A��uA��!A�ȴA��^A�l�A�r�A�{A���A�A{|�Aw�Au&�Apn�Am%Ag;dAb5?A^^5A\bAW�AQ�AM�AI?}AHffAG33AE�;AEO�AD��ADZAB�jA@��A@�A?C�A>  A<bA:ȴA8��A6�jA5l�A5+A5
=A4ĜA4��A3x�A1��A01A/?}A-�A*ĜA)�TA)K�A)/A(=qA'S�A&��A%G�A#�7A!dZAr�A�7A33A��A�HA9XA�A�A�
AC�AbA33AA�A��A~�AE�A�A��AdZAƨAhsA+A{AAA
M�A	oA	&�A	��A
�/A�A/A
�\A
VA	G�AM�A%AS�A��A�9AbNA&�A 5?@��/@�`B@�-@��@�v�@���@�7L@��@�Ĝ@�9X@�~�@�hs@�l�@�@�@ꟾ@�P@�ff@��#@�(�@���@��@���@�X@�^5@�@��@��#@�I�@ݩ�@�G�@�7L@��@���@ܣ�@�ƨ@�&�@�bN@� �@�1@��
@���@�~�@�X@Ӆ@���@�=q@�p�@Л�@�A�@ύP@�v�@Ο�@Χ�@�^5@̴9@̃@ˍP@�"�@�"�@��y@�V@ɩ�@ȣ�@�S�@�-@�p�@��@�ff@���@�?}@���@�j@�r�@��@��D@���@� �@�C�@�o@�"�@�l�@�S�@�~�@�`B@�V@�1'@���@��;@�9X@��!@�=q@�n�@���@�7L@�&�@��@��@�Q�@�1@��
@���@�\)@��@���@�C�@�\)@�K�@�ȴ@�^5@��@��-@�X@�V@���@�(�@�|�@���@�V@�=q@��-@�`B@��h@�hs@��@���@�ƨ@�K�@��@���@�~�@�ff@�^5@�@��@�x�@���@���@��u@�1'@���@�33@�ȴ@�=q@��#@���@�7L@���@�r�@�Q�@��@�dZ@��@�V@��7@��@��h@���@��^@���@��-@��@�7L@��@�1'@��
@�o@���@��R@�V@���@��^@���@�p�@�O�@�G�@�X@�&�@���@���@�j@��@�|�@�33@��@���@�~�@�=q@���@�Ĝ@��@�A�@�9X@�Ĝ@��`@��/@��@��@�ȴ@�v�@�=q@�X@�x�@�7L@���@���@���@�r�@�Q�@�Z@��j@���@��@�1'@�1@��@��m@��m@��m@��
@�ƨ@�t�@���@�{@�{@��@�{@�%@���@�Z@� �@���@�ƨ@��F@���@�t�@�S�@��@���@�ff@�-@��-@��h@��7@��7@��@�7L@�%@��j@�r�@�9X@��;@���@�dZ@�;d@��@�-@��#@�hs@���@��9@�j@�Z@�A�@�1@���@�\)@�33@�
=@���@�~�@�=q@�J@���@��-@���@��@�x�@�x�@�hs@�%G�O�@���@���@x��@pĜ@i�@`A�@Y&�@P  @G;d@A%@9%@1G�@,1@%��@ �u@9X@��@��@��@
~�@K�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�yB�yB�sB�sB�sB�sB�sB�yB�B�B�yB�B�yB�mB�`B�TB�BB�/B�B�
B��B��B��B��B��B��B��B�)B�B�B�B�B�B�B��B	1B	<jB	�B	�^B
B
L�B
� B
��B
��B
�!B
�ZBB�B:^B<jBH�BdZBk�B{�B�+B�B�'B�!B�B�B��B��B��B��B��B��B�\B�hB�VB�PB�=B�Bq�BS�BA�BW
B[#BH�B;dB?}B
�/B
��B
�NB%B�B{B
��B
��B
�9B
��B
��B
��B
��B
��B
�FB
�FB
��B
�B
E�B
�B
VB	��B	�TB	ĜB	�{B	w�B	ffB	L�B	49B	!�B	uB	+B��B�B�`B�)B��B�wB�RB�-B�!B�B�B�B�B�B�B�-B�^B�XB�FB�B��B��B��B�B�BB��B��B�B�
BɺBƨBŢBŢB��B�dB�RB�B��B��B�VB�PB�VB�Bz�B}�B�B�7B�hB�uB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B�7B�B�B�B�1B�bB�BĜBȴBǮBɺB��B��BŢB�LB�B�B�B��B��B��B�oB��B�B�B�RB�^B�RB�LB�?B�9B�?B�-B�!B�B�B��B��B�B��B��B��B��B��B�BÖB��B��B��B��B��B��B��B��B��B�B�
B�B�B�B�B�/B�/B�)B�B�B�/B�BB�B�B�B�B�B��B��B��B��B��B��B	  B	1B	1B	1B	DB	VB	PB	\B	hB	oB	uB	�B	�B	�B	�B	!�B	#�B	&�B	(�B	'�B	(�B	+B	-B	.B	/B	/B	0!B	49B	7LB	A�B	L�B	M�B	M�B	VB	[#B	]/B	]/B	aHB	dZB	gmB	iyB	l�B	o�B	o�B	r�B	u�B	y�B	z�B	{�B	z�B	x�B	w�B	z�B	~�B	}�B	}�B	}�B	}�B	�B	�B	�%B	�B	�+B	�DB	�PB	�PB	�PB	�VB	�bB	�hB	�hB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�-B	�-B	�3B	�?B	�9B	�3B	�3B	�3B	�-B	�9B	�9B	�?B	�FB	�LB	�RB	�RB	�RB	�RB	�RB	�RB	�^B	�dB	�jB	�qB	�qB	�wB	�}B	B	ƨB	ƨB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�/B	�HB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�TB	�HB	�HB	�NB	�HB	�NB	�HB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
bB
�B
�B
(�B
0!B
6FB
?}B
C�B
J�B
Q�B
W
B
]/B
`BB
e`B
k�B
o�B
u�B
v�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�xB�wB�xB�yB�yB�B�B�B�~B�B�}B�uB�cB�[B�IB�6B�#B�B��B��B��B��B��B��B��B�/B�B�B�B�B�B�B��B	7B	<iB	�B	�\B
B
L�B
�B
��B
��B
�B
�LBB�B:OB<]BH�BdIBktB{�B�B�B�B�B�B��B��B��B��B�wB�{B�sB�MB�VB�GB�AB�.B��Bq�BS�BAyBV�B[BH�B;SB?mB
�!B
��B
�ABB�BkB
��B
̼B
�*B
��B
��B
��B
��B
��B
�7B
�7B
��B
��B
E�B
�B
JB	��B	�KB	ĔB	�sB	w�B	f_B	L�B	44B	!�B	tB	)B��B�B�]B�&B��B�vB�RB�,B�B�B��B�B�B�B�B�-B�^B�VB�DB�B��B��B��B�B�BB��B��B� B�BɷBƢBŞBŝB��B�dB�OB�B��B��B�TB�QB�TB�Bz�B}�B�B�4B�hB�uB�zB�yB��B��B��B��B��B��B��B��B��B��B��B��B�6B�B�B�B�2B�_B�BĖBȰBǨBɶB��B��BŞB�HB�B�B�B��B��B��B�kB��B�B�B�LB�YB�KB�GB�<B�4B�=B�'B�B�B�B��B��B��B��B��B��B��B��B�BÓB��B��B��B��B��B��B��B��B��B��B�B�	B�B�B�B�(B�)B�!B�B�B�'B�;B�{B�B�B�B�B��B��B��B��B��B��B��B	*B	'B	+B	<B	MB	HB	UB	]B	fB	mB	xB	�B	�B	�B	!�B	#�B	&�B	(�B	'�B	(�B	*�B	-B	.
B	/B	/B	0B	4,B	7?B	A~B	L�B	M�B	M�B	U�B	[B	] B	] B	a>B	dNB	g_B	ikB	l~B	o�B	o�B	r�B	u�B	y�B	z�B	{�B	z�B	x�B	w�B	z�B	~�B	}�B	}�B	}�B	}�B	��B	��B	�B	�B	�B	�7B	�BB	�CB	�CB	�GB	�UB	�ZB	�XB	�aB	�_B	�hB	�pB	�wB	�yB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�.B	�(B	�#B	�%B	�$B	�B	�*B	�*B	�/B	�5B	�:B	�AB	�@B	�AB	�DB	�CB	�BB	�NB	�VB	�YB	�bB	�_B	�hB	�kB	�}B	ƕB	ƚB	ƔB	ʰB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�8B	�;B	�?B	�DB	�DB	�DB	�BB	�DB	�IB	�BB	�6B	�7B	�:B	�5B	�<B	�6B	�<B	�BB	�DB	�DB	�AB	�CB	�CB	�BB	�BB	�JB	�KB	�IB	�OB	�[B	�ZB	�[B	�[B	�\B	�aB	�_B	�_B	�gB	�fB	�hB	�gB	�jB	�hB	�nB	�sB	�rB	�tB	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B
B
OB
{B
�B
(�B
0B
61B
?jB
C�B
J�B
Q�B
V�B
]B
`.B
eKB
koB
o�B
u�B
v�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.19 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436452016080714364520160807143645  AO  ARCAADJP                                                                    20160330021605    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160330021605  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160330021605  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143645  IP                  G�O�G�O�G�O�                