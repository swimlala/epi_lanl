CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-08-16T09:16:01Z AOML 3.0 creation; 2016-08-07T21:36:38Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150816091601  20160807143638  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               BA   AO  5286_8897_066                   2C  D   APEX                            6531                            072314                          846 @�h
�O�1   @�h
���	@3}/��w�cE�Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    BA   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B?��BH  BP  BXffB_��Bh  Bp  Bx  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA�fDBfDB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD�fD�@ D���D���D���D�)�D�|�D��3D��D�<�D�l�D�ٚD� D�FfD�s3D��fD���D�FfD�3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@�{A
=A#
=AC
=Ac
=A��A��A��A��A��AхA�A�B BBBB!(�B(B0B8B@\)BHBPBY(�B`\)BhBpBxB�aHB��{B�aHB�.B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC 0�C0�C0�C0�C0�C
0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C 0�C"0�C$0�C&0�C(0�C*0�C,0�C.0�C00�C20�C40�C60�C80�C:0�C<0�C>0�C@0�CB0�CD0�CF0�CH0�CJ0�CL0�CN0�CP0�CR0�CT0�CV0�CX0�CZ0�C\0�C^0�C`0�Cb0�Cd0�Cf0�Ch0�Cj0�Cl0�Cn0�Cp0�Cr0�Ct0�Cv0�Cx0�Cz0�C|0�C~0�C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC��C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!)D!�)D")D"�)D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@)D@�)DA)DA��DB�DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dt��Dy��D�zD�FD���D���D��D�/�D���D��GD��D�B�D�r�D�߮D�D�LzD�yGD��zD��D�LzD�GD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�bA�VA��mA��TAݲ-A�O�A�7LA��A���A���A���A�ȴA�AܾwAܼjAܺ^AܶFAܴ9Aܲ-Aܰ!Aܩ�Aܣ�A�x�A�v�A�=qAדuA�  A�ƨAБhAЋDA�  A�7LA�A�9XA�ĜAƕ�Aĕ�A�VA�t�A�\)A�Q�A�9XA��DA�ZA�hsA�/A�$�A��
A�/A�^5A��A��yA��wA�A��A�-A��^A�
=A��A�5?A�ƨA���A�K�A���A�/A�(�A��\A�bA���A�{A��#A���A���A��-A���A�33A���A�M�A�VA���A�VA���A�XA��/A���A�Q�A�E�A��7A�Q�A��;A��wA�9XA���A��9A��\A�hsA��A�"�A��;A�&�A�7LA�A�A��hA���A�l�A�G�A�A��RA��A��#A�ȴA�S�A�A~�`A|ȴAzr�Aw�mAr��Ao�AnM�Al~�AjVAgG�Ab��A]�
AW�TAV�AR�APQ�AO�AN��AJbAGt�AF~�AB��ABbNAAl�A?33A<A9��A8�RA8=qA7�A6��A5&�A4�A3��A2�uA0ĜA/��A/7LA.ĜA-�A,5?A*�yA)\)A)x�A(ffA'�
A'G�A&  A#l�A!��A!��A!C�A!%A {A��Az�A��A�7AO�A��A�;AO�AA�A�AK�A%A1'A��A �A��A��A��A�AI�AS�AhsAjA1AO�AQ�A�A`BA��A$�A�TAdZA
1A	dZA	S�A�RAA$�AXA�/A�#AK�AG�A��A�/A �AK�A �@�ȴ@�bN@�o@�@��`@�I�@�n�@�/@�@�1'@�@�Q�@��@�F@�ff@�h@�-@��@�C�@���@�\@�=q@�@�@�%@���@� �@�I�@��#@�7L@��/@�O�@�7L@�
=@�S�@���@��@�ƨ@��@���@�p�@���@ՙ�@�
=@�ƨ@�hs@ڰ!@�C�@ڰ!@�E�@١�@�/@�t�@ָR@�^5@�1@�X@�n�@ёh@���@ϕ�@�1@�V@�V@��@�b@��H@ȃ@�l�@�C�@�C�@Ƨ�@�^5@���@ģ�@�r�@���@�t�@�33@\@��@�p�@�Ĝ@��@�I�@���@��@�V@���@�?}@�`B@�`B@��@�ƨ@���@�v�@��@�O�@���@���@�ȴ@�E�@��^@�&�@���@�1'@��@��@�o@�
=@��H@�ȴ@���@�M�@��@��@��@��^@�`B@�Ĝ@�A�@�b@��;@���@�l�@�"�@��H@���@��\@�M�@�5?@�5?@���@��#@���@�?}@��/@���@��@�r�@�j@�A�@�1'@�  @�|�@���@���@�33@��@�~�@��+@��@��@��D@���@���@�x�@��#@��-@��7@��@�G�@��h@�/@�V@�`B@��#@��T@�x�@��#@��@��#@��^@�x�@��@��@���@���@�r�@�Q�@�Q�@�1@��@��@�t�@��@�v�@�E�@�J@�@�@���@���@���@��-@�O�@�=q@��@���@�V@�  @�+@�M�@�^5@�+@�(�@�C�@��-@�G�@��@��@���@��P@�K�@��@�ff@��7@� �@�Z@�1'@��F@��
@���@���@�p�@�G�@�/@�9X@���@�S�@�"�@�dZ@�+@���@�;d@��@��@��@�?}@�%@�j@��m@�l�@�S�@���@��m@��F@�33@�"�@��H@��+@�$�@��-@��h@�hs@�V@���@�Z@�9X@���@���@�C�@��@���@�5?@���@�x�@��h@��7@��7@��@�x�@�p�@�;d@�x�@���@~��@r-@hĜ@ct�@]�h@U/@K�
@F$�@=�T@8r�@2�!@,�j@%��@\)@&�@V@M�@v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�bA�VA��mA��TAݲ-A�O�A�7LA��A���A���A���A�ȴA�AܾwAܼjAܺ^AܶFAܴ9Aܲ-Aܰ!Aܩ�Aܣ�A�x�A�v�A�=qAדuA�  A�ƨAБhAЋDA�  A�7LA�A�9XA�ĜAƕ�Aĕ�A�VA�t�A�\)A�Q�A�9XA��DA�ZA�hsA�/A�$�A��
A�/A�^5A��A��yA��wA�A��A�-A��^A�
=A��A�5?A�ƨA���A�K�A���A�/A�(�A��\A�bA���A�{A��#A���A���A��-A���A�33A���A�M�A�VA���A�VA���A�XA��/A���A�Q�A�E�A��7A�Q�A��;A��wA�9XA���A��9A��\A�hsA��A�"�A��;A�&�A�7LA�A�A��hA���A�l�A�G�A�A��RA��A��#A�ȴA�S�A�A~�`A|ȴAzr�Aw�mAr��Ao�AnM�Al~�AjVAgG�Ab��A]�
AW�TAV�AR�APQ�AO�AN��AJbAGt�AF~�AB��ABbNAAl�A?33A<A9��A8�RA8=qA7�A6��A5&�A4�A3��A2�uA0ĜA/��A/7LA.ĜA-�A,5?A*�yA)\)A)x�A(ffA'�
A'G�A&  A#l�A!��A!��A!C�A!%A {A��Az�A��A�7AO�A��A�;AO�AA�A�AK�A%A1'A��A �A��A��A��A�AI�AS�AhsAjA1AO�AQ�A�A`BA��A$�A�TAdZA
1A	dZA	S�A�RAA$�AXA�/A�#AK�AG�A��A�/A �AK�A �@�ȴ@�bN@�o@�@��`@�I�@�n�@�/@�@�1'@�@�Q�@��@�F@�ff@�h@�-@��@�C�@���@�\@�=q@�@�@�%@���@� �@�I�@��#@�7L@��/@�O�@�7L@�
=@�S�@���@��@�ƨ@��@���@�p�@���@ՙ�@�
=@�ƨ@�hs@ڰ!@�C�@ڰ!@�E�@١�@�/@�t�@ָR@�^5@�1@�X@�n�@ёh@���@ϕ�@�1@�V@�V@��@�b@��H@ȃ@�l�@�C�@�C�@Ƨ�@�^5@���@ģ�@�r�@���@�t�@�33@\@��@�p�@�Ĝ@��@�I�@���@��@�V@���@�?}@�`B@�`B@��@�ƨ@���@�v�@��@�O�@���@���@�ȴ@�E�@��^@�&�@���@�1'@��@��@�o@�
=@��H@�ȴ@���@�M�@��@��@��@��^@�`B@�Ĝ@�A�@�b@��;@���@�l�@�"�@��H@���@��\@�M�@�5?@�5?@���@��#@���@�?}@��/@���@��@�r�@�j@�A�@�1'@�  @�|�@���@���@�33@��@�~�@��+@��@��@��D@���@���@�x�@��#@��-@��7@��@�G�@��h@�/@�V@�`B@��#@��T@�x�@��#@��@��#@��^@�x�@��@��@���@���@�r�@�Q�@�Q�@�1@��@��@�t�@��@�v�@�E�@�J@�@�@���@���@���@��-@�O�@�=q@��@���@�V@�  @�+@�M�@�^5@�+@�(�@�C�@��-@�G�@��@��@���@��P@�K�@��@�ff@��7@� �@�Z@�1'@��F@��
@���@���@�p�@�G�@�/@�9X@���@�S�@�"�@�dZ@�+@���@�;d@��@��@��@�?}@�%@�j@��m@�l�@�S�@���@��m@��F@�33@�"�@��H@��+@�$�@��-@��h@�hs@�V@���@�Z@�9X@���@���@�C�@��@���@�5?@���@�x�@��h@��7@��7@��@�x�G�O�@�;d@�x�@���@~��@r-@hĜ@ct�@]�h@U/@K�
@F$�@=�T@8r�@2�!@,�j@%��@\)@&�@V@M�@v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
��B
ɺB
ǮB
��B
��B
�RB
��B�BM�B�LB�BŢB�HB�B>wBQ�Be`Bq�Bx�Bw�B{�B�JB�\B�PB�=B�+B�DB�\B��B�9B��B��B�B�B��BbB�B�BuBPB
=BBBB%B��B�B�B�B�`B�B�#B�)B�B�#B��B�qB�B��B��B�BjB6FB�BbB��B�;B��B�wB�9B��B�7Bs�BZB=qB�BPB
��B
�B
�HB
��B
�wB
��B
��B
��B
�VB
y�B
]/B
;dB
#�B
�B
B	�B	�B	�XB	��B	��B	�PB	� B	gmB	F�B	!�B	B��B�B�`B�HB�BɺB��B�qB�RB�?B�!B�B�B�B�B�B��B��B��B�B�B�-B�9B�3B�?B�FBĜB��B��B��B�B�B�)B�mB�B�#B��B�fB�B�B�sB��B��B��B��B�
B�B�B�/B�ZB�;B�)B�#B�B�5B�NB�HB�5B�B�B��B��B��B��B��B��B��B�;B�/B�`B��B��B��B�B�B��B�B�TB�)B�)B�5B�)B�)B�B�B��B��B�B�B�`B�;B�#B�B��B��B��B��B��B��B�/B�BB�HB�HB�HB�B��B��B	B	1B	PB	\B	hB	�B	�B	�B	�B	JB	B��B	  B	%B	+B��B�B�B�yB�B�B�B�B�B	  B	VB	-B	>wB	8RB	>wB	?}B	F�B	L�B	O�B	H�B	K�B	L�B	D�B	;dB	F�B	E�B	G�B	?}B	B�B	I�B	A�B	9XB	49B	.B	'�B	'�B	+B	.B	/B	2-B	6FB	8RB	7LB	7LB	7LB	7LB	8RB	:^B	>wB	B�B	D�B	H�B	J�B	J�B	K�B	L�B	N�B	P�B	P�B	Q�B	S�B	R�B	R�B	Q�B	Q�B	P�B	P�B	Q�B	R�B	T�B	W
B	ZB	^5B	^5B	]/B	^5B	^5B	`BB	dZB	ffB	ffB	gmB	jB	m�B	n�B	n�B	o�B	r�B	r�B	r�B	s�B	t�B	u�B	u�B	v�B	v�B	w�B	x�B	y�B	}�B	{�B	{�B	{�B	|�B	}�B	~�B	� B	~�B	� B	�B	�B	�%B	�1B	�7B	�PB	�\B	�VB	�oB	�bB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�FB	�^B	�jB	B	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	ɺB	ɺB	��B	ǮB	ÖB	��B	�}B	B	ƨB	��B	��B	ŢB	ŢB	��B	�
B	�
B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�NB	�`B	�sB	�mB	�fB	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
%B
hB
�B
�B
�B
�B
$�B
0!B
5?B
7LB
;dB
B�B
F�B
L�B
VB
\)B
dZB
gmB
k�B
p�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
��B
ɬB
ǣB
��B
��B
�FB
��B�BM�B�?B��BŒB�8B�B>gBQ�BePBq�Bx�Bw�B{�B�;B�PB�AB�)B�B�4B�OB��B�-B��B��B�uB�|B��BRB�B�BgBEB
.BBBBB��B�B�B�yB�PB�	B�B�B��B�B��B�`B�B��B��B�BjoB65B�BPB��B�'BʮB�gB�(B��B�%Bs�BZB=aB�BAB
��B
�|B
�9B
��B
�jB
��B
��B
��B
�GB
y�B
] B
;XB
#�B
�B
B	�B	�B	�NB	��B	��B	�HB	�B	gfB	F�B	!�B	B��B�B�\B�FB�BɺB��B�rB�QB�@B� B�B�B�B�B�B��B��B��B�B�B�*B�6B�0B�AB�DBęB��B��B��B�B�B�$B�hB�B�B��B�_B�B�B�nB��B��B��B��B�B�B�B�*B�VB�8B�$B�B�B�1B�IB�CB�0B�B�B��B��B��B��B��B��B��B�7B�)B�XB��B��B��B�B�B��B�B�NB�#B�%B�/B�"B�#B�xB�B��B��B�B�B�[B�6B�B�
B��B��B��B��B��B��B�'B�<B�BB�CB�AB�~B��B��B	B	*B	GB	SB	aB	wB	B	�B	wB	AB	B��B��B	B	#B��B�B�B�sB�xB�|B�B�B�B��B	NB	-B	>iB	8FB	>jB	?sB	F�B	L�B	O�B	H�B	K�B	L�B	D�B	;YB	F�B	E�B	G�B	?tB	B�B	I�B	AB	9JB	4/B	.
B	'�B	'�B	*�B	.B	/B	2"B	69B	8HB	7?B	7@B	7AB	7@B	8GB	:SB	>jB	B�B	D�B	H�B	J�B	J�B	K�B	L�B	N�B	P�B	P�B	Q�B	S�B	R�B	R�B	Q�B	Q�B	P�B	P�B	Q�B	R�B	T�B	V�B	ZB	^)B	^'B	]#B	^*B	^(B	`6B	dMB	fVB	fZB	gbB	jsB	m�B	n�B	n�B	o�B	r�B	r�B	r�B	s�B	t�B	u�B	u�B	v�B	v�B	w�B	x�B	y�B	}�B	{�B	{�B	{�B	|�B	}�B	~�B	�B	~�B	�B	��B	�B	�B	�!B	�*B	�AB	�LB	�FB	�aB	�SB	�NB	�UB	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�4B	�NB	�XB	�~B	ŒB	ǠB	ɩB	˷B	̼B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɬB	ɩB	��B	ǠB	ÄB	�zB	�lB	B	ƖB	��B	ʲB	őB	őB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	̽B	��B	��B	��B	��B	��B	��B	��B	�B	�;B	�NB	�aB	�[B	�UB	�TB	�UB	�aB	�~B	�}B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
B
UB
�B
�B
�B
�B
$�B
0B
5+B
76B
;PB
B}B
F�B
L�B
U�B
\B
dFB
gXB
krB
p�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.19 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436382016080714363820160807143638  AO  ARCAADJP                                                                    20150816091601    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150816091601  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150816091601  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143638  IP                  G�O�G�O�G�O�                