CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-15T19:27:57Z AOML 3.0 creation; 2016-08-07T21:36:37Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150715192757  20160807143637  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               <A   AO  5286_8897_060                   2C  D   APEX                            6531                            072314                          846 @�`vT2
1   @�`"`��@2>vȴ9X�cO��S��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    <A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB>��BG��BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"�fD#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D=��D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D�	�D�33D�� D��fD�	�D�6fD���D��fD� D�FfD��3D�ٚD�fD�S3D�|�D�ɚD�3D�<�D�fD�p 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�{@�{A
=A#
=AC
=Ac
=A��A��A��A��A��AхA�A�B BBBB B(B0B9(�B?�\BH\)BP\)BXB`BhBpBxB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC 0�C0�C0�C0�C0�C
0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C 0�C"0�C$0�C&0�C(0�C*0�C,0�C.0�C00�C20�C40�C60�C80�C:0�C<0�C>0�C@0�CB0�CD0�CF0�CH0�CJ0�CL0�CN0�CP0�CR0�CT0�CV0�CX0�CZ0�C\0�C^0�C`0�Cb0�Cd0�Cf0�Ch0�Cj0�Cl0�Cn0�Cp0�Cr0�Ct0�Cv0�Cx0�Cz0�C|0�C~0�C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D�D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!)D!�)D")D"��D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>�D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dt�)Dy��D��D�9GD��D��zD��D�<zD���D��zD�D�LzD��GD�߮D�zD�YGDڂ�D�ϮD�GD�B�D�zD�v11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�33A�33A�33A�33A�33A�1'A�/A�1'A�7LA�;dA�?}A�?}A�A�A� �A�VA���A���A�`BA�=qA�oA��AօAԧ�A�A�AГuA·+A�;dAˋDA�&�A�E�AȲ-A�bNA��mAƴ9A�$�Aě�A�A�A�1AøRA�G�A��yA��A���A�7LA��HA�p�A�A�M�A���A���A��\A��A��PA��9A�l�A��9A��wA�;dA��`A��DA�M�A��^A�=qA��A���A�dZA�G�A�A�/A�;dA�ZA��A�{A��hA�S�A���A�G�A�9XA���A���A�1'A�VA��-A��TA�-A��FA�  A�I�A�r�A�$�A��A��jA��A�bNA�x�A���A�ƨA��A�M�A�~�A��!A�7LA��A��!A��A��A��wA���A���A~jAzȴAu|�As7LAk�FAgx�Ad�Aa7LA^��A]�AY�
AU;dAR(�AQAO��AL��AJ��AI|�AG�AFM�ADZAA�hA?"�A=�
A:��A8jA7�
A7A6VA5K�A2�A.��A.~�A.z�A.VA-XA+�#A)��A(�A(ffA'�A'x�A'�A%|�A ��A�FA�uA�A��AXAA��A&�AI�A�A�AhsA�AjAƨA�-Av�A�\A�A��Az�A �Al�A��AM�A�A��A��AK�A�AoA
�A
^5A	+A��A�A�PA�A�jA�
A;dA&�A��A�HA�yA��A�RA��AjAp�A bNA �A E�@���@���@�E�@���@��@�/@�b@���@���@�!@�G�@��
@��@�h@�9X@�v�@�J@�j@�(�@�-@�j@��@�S�@�@�F@�9@�\)@�`B@���@��@�;d@�Ĝ@�?}@���@�z�@�1@�C�@�b@�!@��@���@�+@�|�@�@�1'@�$�@�x�@���@�o@Ԭ@�|�@�1@�$�@Ѳ-@϶F@́@�b@��@�b@�ff@̋D@���@�j@�/@�ff@��#@�%@�%@��@��/@�p�@�$�@��;@�ƨ@�M�@��#@�A�@ǥ�@ȣ�@�`B@ɡ�@Ɂ@ɑh@ɲ-@���@�M�@�o@��@ȃ@ȋD@�%@��@�/@ț�@�l�@��H@��@�{@���@ă@�7L@�1'@�Z@�z�@�r�@�A�@��@���@���@�x�@�G�@�?}@��@�(�@��@��@��!@�V@�$�@�J@�@���@�G�@�%@��`@��@��@� �@���@��;@�t�@�o@�V@��#@�O�@�V@���@��@��/@���@��/@���@�Ĝ@�1'@�ƨ@���@�;d@�;d@��y@���@��@��F@���@�C�@�@���@���@�~�@�ff@�E�@�J@��@�@�X@��@�bN@��@���@��m@��;@��@�\)@��y@��!@��+@�J@��@�G�@�&�@�%@��`@���@�r�@�b@���@�33@���@��!@�v�@�5?@���@��-@���@��7@�x�@�hs@�X@�/@��`@���@�Z@� �@��@��m@��F@��@���@�l�@��@��R@��^@���@��j@�Q�@�dZ@�33@�o@��y@�v�@�J@���@��7@�/@��@�Z@�b@��w@�\)@�C�@�+@�o@���@�ȴ@�^5@�$�@�$�@�@���@��^@���@�p�@�?}@��@��`@��j@�bN@��@��m@�ƨ@��F@�;d@���@��@��+@�-@���@�&�@��@���@�Ĝ@�z�@�Z@�j@�9X@���@�ƨ@��;@���@���@��
@��@���@�\)@��+@�-@�J@��#@��^@���@��7@�X@�@���@���@�;@u�@m?}@c�F@X�@P��@Hr�@@Q�@8r�@0Ĝ@,I�@'�@#@O�@��@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�33A�33A�33A�33A�33A�1'A�/A�1'A�7LA�;dA�?}A�?}A�A�A� �A�VA���A���A�`BA�=qA�oA��AօAԧ�A�A�AГuA·+A�;dAˋDA�&�A�E�AȲ-A�bNA��mAƴ9A�$�Aě�A�A�A�1AøRA�G�A��yA��A���A�7LA��HA�p�A�A�M�A���A���A��\A��A��PA��9A�l�A��9A��wA�;dA��`A��DA�M�A��^A�=qA��A���A�dZA�G�A�A�/A�;dA�ZA��A�{A��hA�S�A���A�G�A�9XA���A���A�1'A�VA��-A��TA�-A��FA�  A�I�A�r�A�$�A��A��jA��A�bNA�x�A���A�ƨA��A�M�A�~�A��!A�7LA��A��!A��A��A��wA���A���A~jAzȴAu|�As7LAk�FAgx�Ad�Aa7LA^��A]�AY�
AU;dAR(�AQAO��AL��AJ��AI|�AG�AFM�ADZAA�hA?"�A=�
A:��A8jA7�
A7A6VA5K�A2�A.��A.~�A.z�A.VA-XA+�#A)��A(�A(ffA'�A'x�A'�A%|�A ��A�FA�uA�A��AXAA��A&�AI�A�A�AhsA�AjAƨA�-Av�A�\A�A��Az�A �Al�A��AM�A�A��A��AK�A�AoA
�A
^5A	+A��A�A�PA�A�jA�
A;dA&�A��A�HA�yA��A�RA��AjAp�A bNA �A E�@���@���@�E�@���@��@�/@�b@���@���@�!@�G�@��
@��@�h@�9X@�v�@�J@�j@�(�@�-@�j@��@�S�@�@�F@�9@�\)@�`B@���@��@�;d@�Ĝ@�?}@���@�z�@�1@�C�@�b@�!@��@���@�+@�|�@�@�1'@�$�@�x�@���@�o@Ԭ@�|�@�1@�$�@Ѳ-@϶F@́@�b@��@�b@�ff@̋D@���@�j@�/@�ff@��#@�%@�%@��@��/@�p�@�$�@��;@�ƨ@�M�@��#@�A�@ǥ�@ȣ�@�`B@ɡ�@Ɂ@ɑh@ɲ-@���@�M�@�o@��@ȃ@ȋD@�%@��@�/@ț�@�l�@��H@��@�{@���@ă@�7L@�1'@�Z@�z�@�r�@�A�@��@���@���@�x�@�G�@�?}@��@�(�@��@��@��!@�V@�$�@�J@�@���@�G�@�%@��`@��@��@� �@���@��;@�t�@�o@�V@��#@�O�@�V@���@��@��/@���@��/@���@�Ĝ@�1'@�ƨ@���@�;d@�;d@��y@���@��@��F@���@�C�@�@���@���@�~�@�ff@�E�@�J@��@�@�X@��@�bN@��@���@��m@��;@��@�\)@��y@��!@��+@�J@��@�G�@�&�@�%@��`@���@�r�@�b@���@�33@���@��!@�v�@�5?@���@��-@���@��7@�x�@�hs@�X@�/@��`@���@�Z@� �@��@��m@��F@��@���@�l�@��@��R@��^@���@��j@�Q�@�dZ@�33@�o@��y@�v�@�J@���@��7@�/@��@�Z@�b@��w@�\)@�C�@�+@�o@���@�ȴ@�^5@�$�@�$�@�@���@��^@���@�p�@�?}@��@��`@��j@�bN@��@��m@�ƨ@��F@�;d@���@��@��+@�-@���@�&�@��@���@�Ĝ@�z�@�Z@�j@�9X@���@�ƨ@��;@���@���@��
@��@���@�\)@��+@�-@�J@��#@��^@���@��7G�O�@�@���@���@�;@u�@m?}@c�F@X�@P��@Hr�@@Q�@8r�@0Ĝ@,I�@'�@#@O�@��@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
dZB
dZB
dZB
gmB
n�B
w�B
�B
��B
��B%B.B�%B�XB�sBVB(�B:^B?}BG�B]/Bt�B|�B�B�+B�JB�{B�!BÖB��B�NB�B�B�B��BBB��B��B��B��BBBBDB�B,B8RB=qBP�BXBVBQ�BL�BI�BE�B>wB,B�B
=B��B��B�B�sB�NB�/B��B�wB�-B��B��B�hB�DB�Bz�BjBdZB`BB[#BK�B33B�B1B��Bk�B0!B�B
��B
ȴB
�FB
��B
�7B
gmB
S�B
@�B
�B	��B	�HB	�^B	��B	v�B	\)B	L�B	9XB	)�B	�B	
=B��B�B�yB�ZB�;B�B��B��B��BƨB��B�}B�qB�LB�-B�'B�-B�3B�3B�RB�jB�wB�wB�qB��BĜB��B�`B��B��B��B��B�B�B��BɺBǮBǮBŢBÖB��BBŢBƨBȴB��B��B��B�
B�B�
B��B��B��B��BɺB��B�
B�
B��B��B��B��BɺBȴBƨBĜB�qB�-B��B��B�B�B�B�B�B�!B�^B��BĜBǮB��B��BŢBB�B�B��B��B�/B�B�B�;B�TB�;B�/B�)B�NB�TB�HB�5B�#B�B�B�B�#B�
B��B��B��B�HB	B	�B	hB	PB	(�B	F�B	E�B	;dB	.B	.B	0!B	0!B	33B	=qB	?}B	C�B	H�B	Q�B	H�B	D�B	>wB	7LB	33B	)�B	�B	33B	N�B	Q�B	;dB	C�B	<jB	5?B	2-B	:^B	M�B	I�B	H�B	H�B	C�B	H�B	R�B	Q�B	O�B	P�B	T�B	T�B	ZB	aHB	m�B	m�B	hsB	gmB	dZB	cTB	l�B	r�B	s�B	t�B	u�B	y�B	z�B	w�B	�B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�bB	�uB	��B	��B	��B	��B	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�XB	�dB	�^B	�RB	�RB	�^B	�dB	�dB	�dB	�qB	��B	��B	ÖB	B	B	��B	ÖB	ĜB	ĜB	��B	�wB	�wB	�}B	��B	B	ÖB	ÖB	ĜB	ĜB	ĜB	ŢB	ŢB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�5B	�;B	�;B	�NB	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�mB	�mB	�yB	�yB	�yB	�sB	�mB	�mB	�sB	�yB	�sB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
hB
�B
�B
$�B
)�B
.B
6FB
=qB
C�B
H�B
M�B
T�B
YB
]/B
cTB
dZB
l�B
q�B
t�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
eXB
eVB
eXB
eXB
eWB
eXB
eVB
eWB
eXB
eUB
eUB
eWB
eWB
eVB
dSB
dQB
dQB
gdB
n�B
w�B
�B
��B
��BB.B�B�IB�dBEB(�B:NB?kBG�B]Bt�B|�B�B�B�:B�iB�BÅB��B�<B�B�B�B��B�B �B��B��B��B��B�B
B�B6B�B+�B8BB=cBP�BXBU�BQ�BL�BI�BE�B>iB+�B}B
-B��B��B�B�gB�=B�B��B�gB�B��B��B�QB�4B�Bz�BjnBdGB`.B[BK�B3#B�BB˳BksB0B|B
��B
ȥB
�6B
��B
�'B
g\B
S�B
@vB
�B	��B	�?B	�VB	��B	v�B	\$B	L�B	9RB	)�B	�B	
9B��B�B�vB�YB�7B�B��B��B��BƥB��B�{B�mB�IB�-B�&B�,B�2B�3B�OB�fB�tB�tB�lB��BęB��B�YB��B��B��B��B�B��B��BɴBǪBǪBŜBÔB��BBŠBƥBȰBʿB��B��B�B�B�B��BʽBʽB��BɶB��B�B�B��B��B��B��BɶBȱBƣBęB�mB�(B��B��B��B�B��B��B�B�B�YB�}BĘBǧBʾBʿBŝBB��B��B��B��B�)B�
B�B�3B�LB�4B�'B�#B�JB�MB�BB�0B�B�B�B�B�B�B��B��B��B�AB	B	�B	_B	GB	(�B	F�B	E�B	;\B	.B	.B	0B	0B	3'B	=eB	?rB	C�B	H�B	Q�B	H�B	D�B	>jB	7AB	3)B	)�B	�B	3'B	N�B	Q�B	;YB	C�B	<_B	52B	2"B	:SB	M�B	I�B	H�B	H�B	C�B	H�B	R�B	Q�B	O�B	P�B	T�B	T�B	ZB	a;B	m�B	m�B	hjB	g`B	dNB	cJB	l~B	r�B	s�B	t�B	u�B	y�B	z�B	w�B	��B	�\B	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�gB	�TB	�gB	�yB	�}B	�~B	�sB	�HB	�YB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�GB	�XB	�NB	�@B	�AB	�OB	�UB	�XB	�VB	�aB	�vB	�zB	ÅB	�B	B	�yB	ÄB	ČB	ĊB	�tB	�iB	�gB	�nB	�xB	�~B	ÅB	ÆB	ċB	čB	čB	œB	őB	ƕB	ǞB	ǞB	ɬB	ʲB	˴B	˶B	˴B	˷B	˶B	˶B	˷B	ʰB	ʲB	˹B	˸B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�&B	�+B	�(B	�9B	�SB	�\B	�[B	�\B	�\B	�ZB	�bB	�bB	�`B	�[B	�ZB	�hB	�iB	�hB	�cB	�]B	�[B	�`B	�jB	�aB	�[B	�fB	�pB	�tB	�yB	�xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 B
�B
B

B
B
B
B
B
B
B
G�O�B
B
UB
qB
�B
$�B
)�B
.B
62B
=]B
C�B
H�B
M�B
T�B
YB
]B
c@B
dEB
lvB
q�B
t�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.19 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436372016080714363720160807143637  AO  ARCAADJP                                                                    20150715192757    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150715192757  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150715192757  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143637  IP                  G�O�G�O�G�O�                