CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-31T19:17:29Z AOML 3.0 creation; 2016-08-07T21:36:37Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150731191729  20160807143637  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ?A   AO  5286_8897_063                   2C  D   APEX                            6531                            072314                          846 @�dqf 1   @�d�`@3�-V�c7��S��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ?A   B   B   @y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B��B��B(  B0  B8ffB@ffBG��BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D y�D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3DyY�D�3D�@ D�s3D�� D�  D�<�D��3D��3D��D�<�D�vfD��3D� D�P D�vfD���D� D�FfD�|�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=q@�p�A�RA"�RAB�RAb�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B �B�Bz�BG�B G�B(�B0�B9zBAzBHG�BPG�BX�B`�Bh�Bp�Bx�B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
C +�C+�C+�C+�C+�C
+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C +�C"+�C$+�C&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf+�Ch+�Cj+�Cl+�Cn+�Cp+�Cr+�Ct+�CvECxECz+�C|+�C~+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D 
�D ��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D	
�D	��D

�D
��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D {D �{D!
�D!��D"
�D"��D#
�D#��D$
�D$��D%
�D%��D&
�D&��D'
�D'��D(
�D(��D)
�D)��D*
�D*��D+
�D+��D,
�D,��D-
�D-��D.
�D.��D/
�D/��D0
�D0��D1
�D1��D2
�D2��D3
�D3��D4
�D4��D5
�D5��D6
�D6��D7
�D7��D8
�D8��D9
�D9��D:
�D:��D;
�D;��D<
�D<��D=
�D=��D>
�D>��D?
�D?��D@
�D@��DA
�DA��DB
�DB��DC
�DC��DD
�DD��DE
�DE��DF
�DF��DG
�DG��DH
�DH��DI
�DI��DJ
�DJ��DK
�DK��DL
�DL��DM
�DM��DN
�DN��DO
�DO��DP
�DP��DQ
�DQ��DR
�DR��DS
�DS��DT
�DT��DU
�DU��DV
�DV��DW
�DW��DX
�DX��DY
�DY��DZ
�DZ��D[
�D[��D\
�D\��D]
�D]��D^
�D^��D_
�D_��D`
�D`��Da
�Da��Db
�Db��Dc
�Dc��Dd
�Dd��De
�De��Df
�Df��Dg
�Dg��Dh
�Dh��Di
�Di��Dj
�Dj��Dk
�Dk��Dl
�Dl��Dm
�Dm��Dn
�Dn��Do
�Do��Dp
�Dp��Dq
�Dq��Dr
�Dr��Ds
�Ds��Dt
�Dt~Dyd{D��D�EqD�x�D��qD�qD�B>D���D�ȤD�">D�B>D�{�D�ؤD�qD�UqD�{�D��>D�qD�K�D�>D��>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A��
A���A���A���A���A��
A��A��
A��A���A���A�ĜA�ȴA۴9A��Aُ\A�\)A�~�A��#A��A�p�AΏ\A�K�A�ZA�/A�I�A�A�C�A�|�A�XA�5?AčPA�G�A���A���A�r�A�%A��TA��/A���A�;dA�I�A�G�A���A�=qA�=qA���A�K�A�p�A�VA�`BA���A�1A�r�A��!A�~�A��^A��HA�JA��DA��A��A��A�S�A��A�`BA���A��A�%A�dZA�/A���A��A�{A�G�A�z�A�;dA��yA�-A�O�A���A���A�E�A�M�A�A��A��;A��\A�7LA�-A���A��+A�A���A��TA��A���A��yA��A��HA��A�G�A�1A�~�A�
=A�O�A���A�A}��A}
=A|E�Ayt�Aw+At�uAr�yAp��AnjAl�Ajv�AiS�Ah��Ah(�Af�/Ac�mAb=qA`�`A^jA[��AVz�APZAM��ALv�AK33AH9XAG7LAE�AD1'AB�9AAA?��A;��A;�A:�A9l�A8bA6VA3��A25?A1�A0$�A.��A,��A+A*VA(�A&��A%A%�hA%��A%��A%XA$��A$(�A"�/A!�
A 1'A��A�HA �A�7AQ�A;dA�AC�AQ�Ax�A\)A&�A^5AbA{A�A{AA�AI�AS�Al�AbA�AK�AA/AA�A�A?}A?}Ap�A��A��Al�A
�jA
E�A
��A	��A~�AJA1AƨA�`A�FA��AI�AJAG�AĜAZA�#A �uA �@���@��H@��\@�7L@�o@��-@��@�9X@��@��@��u@���@���@�o@��T@�V@�33@���@땁@�$�@�J@�O�@�@�V@��/@���@���@�$�@��T@��#@�hs@���@��T@ݲ-@�?}@���@��@�~�@���@؋D@��m@�K�@֏\@���@�&�@���@�o@���@҇+@���@�Ĝ@�5?@�  @��
@˾w@˅@ȼj@�1@ǝ�@���@�o@�@�@ƸR@���@þw@¸R@���@¸R@\@+@� �@��@��!@�@��@��`@��@��@���@�
=@�@�ff@�J@��T@�{@��@�`B@��@��F@�V@�M�@�z�@��@�(�@�x�@�=q@�x�@��@���@��7@�K�@�M�@�=q@��D@�x�@�I�@��F@��@�/@���@��`@�x�@�l�@�~�@���@�E�@�-@�E�@�^5@�^5@���@�O�@�7L@��@���@���@�A�@��@���@�\)@�K�@�S�@�;d@���@��R@���@��+@�^5@��@�7L@���@��@��7@��@���@�Ĝ@���@��@�p�@��@��@�bN@�z�@�z�@�9X@�+@�M�@��@�l�@��@��P@�j@�&�@���@�bN@��j@���@���@�~�@��R@�(�@�(�@� �@��m@�|�@��y@�n�@�M�@���@�@��@�@��@���@�x�@�O�@�G�@��@�j@�  @��;@���@���@�dZ@�
=@�$�@��`@�z�@�9X@� �@� �@��@��!@�5?@��@��T@��T@��#@�v�@��+@�E�@�7L@�&�@�G�@��7@���@��@� �@���@�S�@���@���@�E�@��@���@�X@��@�Ĝ@���@�A�@�1'@�  @���@�l�@��@��R@���@��\@�~�@�ff@�=q@��T@��7@��@�1'@� �@�1@�ƨ@�t�@�\)@�33@�
=@���@�E�@��@��-@�p�@�`B@�G�@�V@��`@��/@���@���@��u@��R@��R@�z�@{�@q�@n�R@f@^{@V{@N@F��@?\)@9&�@1��@+ƨ@&$�@ ��@1@l�@��@�P11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A���A���A���A��
A���A���A���A���A��
A��A��
A��A���A���A�ĜA�ȴA۴9A��Aُ\A�\)A�~�A��#A��A�p�AΏ\A�K�A�ZA�/A�I�A�A�C�A�|�A�XA�5?AčPA�G�A���A���A�r�A�%A��TA��/A���A�;dA�I�A�G�A���A�=qA�=qA���A�K�A�p�A�VA�`BA���A�1A�r�A��!A�~�A��^A��HA�JA��DA��A��A��A�S�A��A�`BA���A��A�%A�dZA�/A���A��A�{A�G�A�z�A�;dA��yA�-A�O�A���A���A�E�A�M�A�A��A��;A��\A�7LA�-A���A��+A�A���A��TA��A���A��yA��A��HA��A�G�A�1A�~�A�
=A�O�A���A�A}��A}
=A|E�Ayt�Aw+At�uAr�yAp��AnjAl�Ajv�AiS�Ah��Ah(�Af�/Ac�mAb=qA`�`A^jA[��AVz�APZAM��ALv�AK33AH9XAG7LAE�AD1'AB�9AAA?��A;��A;�A:�A9l�A8bA6VA3��A25?A1�A0$�A.��A,��A+A*VA(�A&��A%A%�hA%��A%��A%XA$��A$(�A"�/A!�
A 1'A��A�HA �A�7AQ�A;dA�AC�AQ�Ax�A\)A&�A^5AbA{A�A{AA�AI�AS�Al�AbA�AK�AA/AA�A�A?}A?}Ap�A��A��Al�A
�jA
E�A
��A	��A~�AJA1AƨA�`A�FA��AI�AJAG�AĜAZA�#A �uA �@���@��H@��\@�7L@�o@��-@��@�9X@��@��@��u@���@���@�o@��T@�V@�33@���@땁@�$�@�J@�O�@�@�V@��/@���@���@�$�@��T@��#@�hs@���@��T@ݲ-@�?}@���@��@�~�@���@؋D@��m@�K�@֏\@���@�&�@���@�o@���@҇+@���@�Ĝ@�5?@�  @��
@˾w@˅@ȼj@�1@ǝ�@���@�o@�@�@ƸR@���@þw@¸R@���@¸R@\@+@� �@��@��!@�@��@��`@��@��@���@�
=@�@�ff@�J@��T@�{@��@�`B@��@��F@�V@�M�@�z�@��@�(�@�x�@�=q@�x�@��@���@��7@�K�@�M�@�=q@��D@�x�@�I�@��F@��@�/@���@��`@�x�@�l�@�~�@���@�E�@�-@�E�@�^5@�^5@���@�O�@�7L@��@���@���@�A�@��@���@�\)@�K�@�S�@�;d@���@��R@���@��+@�^5@��@�7L@���@��@��7@��@���@�Ĝ@���@��@�p�@��@��@�bN@�z�@�z�@�9X@�+@�M�@��@�l�@��@��P@�j@�&�@���@�bN@��j@���@���@�~�@��R@�(�@�(�@� �@��m@�|�@��y@�n�@�M�@���@�@��@�@��@���@�x�@�O�@�G�@��@�j@�  @��;@���@���@�dZ@�
=@�$�@��`@�z�@�9X@� �@� �@��@��!@�5?@��@��T@��T@��#@�v�@��+@�E�@�7L@�&�@�G�@��7@���@��@� �@���@�S�@���@���@�E�@��@���@�X@��@�Ĝ@���@�A�@�1'@�  @���@�l�@��@��R@���@��\@�~�@�ff@�=q@��T@��7@��@�1'@� �@�1@�ƨ@�t�@�\)@�33@�
=@���@�E�@��@��-@�p�@�`B@�G�@�V@��`@��/@���@���G�O�@��R@��R@�z�@{�@q�@n�R@f@^{@V{@N@F��@?\)@9&�@1��@+ƨ@&$�@ ��@1@l�@��@�P11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
<jB
<jB
;dB
;dB
;dB
<jB
=qB
=qB
;dB
:^B
:^B
:^B
:^B
9XB
8RB
8RB
7LB
0!B
0!B
bNB
�uB
:^B
$�B
l�B
�uB
ĜB2-BZB��BPB�B5?B=qB2-B/BhB��B��B�BǮB��B��B��B�B�sB�B��BoB�B�B�B�B�B�B�B{BhBJB��B��B��B��B��BBBhB2-B6FB+B!�BoB%B��B��B��B�B�sB�/BǮB��B�dB�?B��B��B�VB�+By�Bq�Bn�Bk�BcTBL�B8RB+BuB%B�BǮB�?B�B�JBjBG�B6FB&�BhB
�B
��B
��B
� B
O�B
@�B
6FB
+B
�B
%B	�B	�HB	��B	��B	�!B	��B	��B	��B	�uB	�=B	x�B	k�B	]/B	E�B	.B	B�
BƨB�qB�FB�LB�wB�}B��BÖB��BB��B�}BBB��B��BŢBɺB��B�B�B�B�
B��BǮBĜBƨBȴB��B��B��B�B��BȴBŢBŢB�}B��BÖBÖBǮBƨBŢBĜBÖBB��B��B��B�#B�TB�yB�B��B��B��B�B�B�B	PB	�B	�B	hB	
=B��B	B	JB	bB	hB	oB	PB	JB	{B	{B	�B	�B	�B	�B	{B	\B	PB	PB	JB	
=B		7B	PB	VB	
=B	DB	PB	JB	
=B	+B	B	  B��B��B		7B	oB	�B	�B	uB	hB	{B	{B	hB	oB	uB	�B	�B	�B	#�B	$�B	&�B	'�B	)�B	+B	+B	+B	+B	,B	/B	/B	/B	49B	6FB	7LB	6FB	8RB	<jB	=qB	<jB	=qB	<jB	>wB	@�B	?}B	?}B	@�B	D�B	C�B	?}B	?}B	?}B	@�B	C�B	E�B	G�B	M�B	S�B	S�B	T�B	XB	XB	R�B	N�B	N�B	N�B	M�B	K�B	I�B	J�B	K�B	O�B	O�B	P�B	XB	^5B	aHB	gmB	k�B	iyB	iyB	m�B	p�B	q�B	q�B	t�B	�B	�VB	��B	��B	��B	��B	�B	�FB	�?B	�3B	�3B	�wB	ȴB	ÖB	�3B	�B	��B	��B	��B	��B	��B	��B	�hB	�B	|�B	{�B	z�B	}�B	� B	�B	�B	�%B	�DB	�PB	�PB	�PB	�JB	�JB	�PB	�PB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�?B	�?B	�9B	�RB	�^B	�^B	�XB	�FB	�-B	�B	��B	��B	�B	�-B	�dB	��B	�}B	B	��B	��B	�B	�
B	�TB	�ZB	�ZB	�ZB	�TB	�NB	�HB	�HB	�TB	�TB	�fB	�yB	�sB	�sB	�mB	�mB	�sB	�yB	�sB	�mB	�mB	�mB	�yB	�B	�yB	�mB	�`B	�ZB	�ZB	�`B	�`B	�`B	�TB	�TB	�TB	�TB	�ZB	�`B	�yB	�B	�yB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
uB
uB
uB
�B
�B
�B
/B
7LB
>wB
C�B
G�B
K�B
R�B
W
B
_;B
aHB
e`B
iyB
m�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
<eB
<eB
;^B
;^B
;^B
<eB
=hB
=iB
;^B
:YB
:WB
:ZB
:ZB
9PB
8NB
8KB
7FB
0B
0B
bIB
�jB
:XB
$�B
l�B
�mB
ēB2 BZB��BCB�B53B=bB2 B/BXB��B��B�BǠB��B��B��B� B�hB�rB��BcB�B�B�B�B�B�B}BoB[B>B��B��B��B��B��BBB_B2#B68B*�B!�B_BB��B��B��B�B�fB� BǟB�zB�SB�1B��B��B�HB�By�Bq�Bn�BkvBcCBL�B8EB*�BdBB�BǞB�/B��B�9BjoBG�B65B&�B[B
�B
��B
��B
�B
O�B
@uB
6<B
*�B
�B
B	�B	�@B	��B	�}B	�B	��B	��B	��B	�nB	�6B	x�B	k�B	]*B	E�B	.B	B�
BƨB�sB�KB�OB�zB�B��BÙB��BB��B�}BBB��B��BšBɹB��B�B�B�B�
B��BǫBĜBƦBȴB��B��B��B�B��BȶBšBšB�{B��B×BÖBǫBƦBşBĝBÔBB��B��B��B�"B�PB�xB�B��B��B��B�B�{B�B	JB	�B	�B	bB	
7B��B	B	GB	^B	dB	iB	LB	CB	wB	uB	}B	�B	�B	�B	vB	TB	LB	KB	CB	
7B		2B	JB	RB	
5B	=B	IB	DB	
9B	$B	
B��B��B��B		0B	iB	�B	�B	nB	cB	uB	uB	aB	jB	oB	�B	�B	�B	#�B	$�B	&�B	'�B	)�B	*�B	*�B	*�B	*�B	,B	/B	/B	/B	41B	6>B	7DB	6?B	8KB	<aB	=gB	<aB	=jB	<bB	>oB	@zB	?vB	?tB	@{B	D�B	C�B	?vB	?sB	?uB	@{B	C�B	E�B	G�B	M�B	S�B	S�B	T�B	XB	XB	R�B	N�B	N�B	N�B	M�B	K�B	I�B	J�B	K�B	O�B	O�B	P�B	XB	^(B	a=B	gdB	k|B	ioB	iqB	m�B	p�B	q�B	q�B	t�B	�B	�IB	��B	��B	��B	��B	�B	�:B	�2B	�'B	�(B	�lB	ȦB	ÈB	�&B	��B	��B	��B	��B	��B	��B	��B	�\B	�B	|�B	{�B	z�B	}�B	�B	�B	�B	�B	�8B	�HB	�EB	�FB	�=B	�=B	�DB	�FB	�OB	�]B	�qB	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�&B	�1B	�2B	�-B	�FB	�QB	�PB	�MB	�7B	�B	��B	��B	��B	��B	� B	�WB	�vB	�oB	B	ʳB	��B	��B	��B	�FB	�JB	�LB	�JB	�EB	�?B	�8B	�8B	�FB	�FB	�YB	�lB	�fB	�fB	�]B	�aB	�eB	�jB	�dB	�]B	�^B	�^B	�lB	�pB	�jB	�`B	�QB	�IB	�JB	�PB	�PB	�SB	�GB	�GB	�FB	�FB	�JB	�PB	�jB	�pB	�iB	�^B	�dB	�jB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 B
B
B
G�O�B
!B
fB
eB
eB
pB
wB
�B
/B
7<B
>fB
C�B
G�B
K�B
R�B
V�B
_)B
a4B
eNB
iiB
m�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.17 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436372016080714363720160807143637  AO  ARCAADJP                                                                    20150731191729    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150731191729  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150731191729  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143637  IP                  G�O�G�O�G�O�                