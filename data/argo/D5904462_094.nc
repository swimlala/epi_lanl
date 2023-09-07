CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-12-22T10:20:58Z AOML 3.0 creation; 2016-08-07T21:51:24Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151222102058  20160807145124  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ^A   AO  5287_9017_094                   2C  D   APEX                            6529                            072314                          846 @׈AAi1   @׈�f�@0P ě���d�V�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ^A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B�ffB�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtFfDyS3D��fD�Y�D��3D��3D���D�FfD���D���D��D�S3D�y�Dǩ�D��D�I�D�|�D���D�fD�I�D�p D�ɚ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@�{A
=A'
=AG
=Ag
=A��A��A��A��AÅAӅA�A�BB	BBB!B)B1B9BABIBQ\)BYBaBiBqByB��HB��HB��HB��HB�{B�{B�G�B��HB��HB��HB�{B��B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B��HB��HB��HB��HB��HB��HB��HB��HB��HC p�Cp�Cp�Cp�Cp�C
p�Cp�Cp�Cp�C�>Cp�Cp�Cp�Cp�Cp�Cp�C p�C"p�C$p�C&p�C(p�C*p�C,p�C.p�C0p�C2p�C4p�C6p�C8p�C:p�C<p�C>p�C@p�CBp�CDp�CFp�CHp�CJp�CLp�CNp�CPp�CRp�CTp�CVp�CXp�CZp�C\p�C^p�C`p�Cbp�Cdp�Cfp�Chp�Cjp�Clp�Cnp�Cpp�Crp�Ctp�Cvp�Cxp�Czp�C|p�C~p�C�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�+�C�+�C�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D �D �)D!)D!�)D")D"�)D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dtb�Dyo\D�zD�g�D��GD��GD�
�D�TzD���D���D�*�D�aGD���DǷ�D�'�D�W�Dڊ�D���D�zD�W�D�~D�׮11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AԓuAԓuAԓuAԑhAԑhAԇ+AԓuAԝ�Aԝ�Aԝ�Aԡ�Aԡ�Aԣ�Aԡ�Aԛ�Aԝ�Aԝ�Aԝ�Aԡ�Aԥ�Aԥ�Aԥ�Aԣ�Aԡ�Aԡ�Aԏ\A�~�A�r�A�p�A�p�A�n�A�n�A�t�A�t�A�v�A�\)A���A��`A��`A���Ȁ\A�oA�/A�C�A���A��/A�~�A�l�A�
=A�ffA�ZA��;A�bNA�I�A��9A���A�K�A��HA�C�A�=qA���A��A��!A�  A�dZA�dZA�-A�l�A��HA��A�A���A��^A��\A��-A�&�A�I�A��A���A�M�A�ĜA��A�n�A�
=A��HA�/A���A�1A���A�C�A��mA�^5A�t�A�-A���A�bNA��A�;dA��A��A��7A���A��^A��A�z�A���A|�AxAv9XAs�#Ar�An$�Aj�AfJAb�!Aap�A_�hA]��A[�mAW�AS�wAQoAN�AL5?AI�hAGS�AB�yA>n�A<^5A;�hA:�uA8��A7�TA7l�A6��A6~�A5\)A3��A2I�A0��A/�mA.v�A-�7A-&�A,^5A+�A*��A)�
A(�`A'�A&�A%�^A$�DA#��A#�A#
=A"�HA"��A"JA!�hA bNA��A�\A��A��A�A33A�mA?}A �A��AO�AE�A�AƨA�hA;dA��A��AA�A��At�A�AVA	|�A�FA
=A��AbNA��AS�At�AZA+A �\@��
@��;@��R@��@��T@���@�1'@���@���@�G�@�;d@�5?@�r�@�j@�5?@�K�@��@��T@��#@�@柾@�-@��@��@��;@�Ĝ@�
=@�A�@�S�@��@���@ى7@���@�V@�r�@�Q�@���@�S�@���@���@ְ!@֟�@֏\@�ff@�V@�E�@��@Ձ@Դ9@�r�@�9X@�1'@��m@�\)@�33@��@�@�
=@�x�@�b@Ͼw@�o@Χ�@�p�@̓u@�\)@��@���@�C�@�
=@���@�=q@��@�?}@�%@���@ț�@���@���@�r�@��@���@ǥ�@�33@��@Ɵ�@�v�@�M�@�-@�@őh@�hs@�p�@�7L@�bN@���@�\)@���@�@�$�@��@�X@�V@��@��@��@�S�@��H@��\@�ff@�-@�@�G�@��`@��D@�(�@��@��H@�^5@���@�/@�&�@��/@��@���@� �@��w@��@�"�@��@���@��+@�5?@���@��@��m@��@��@�;d@�=q@��h@�/@���@�z�@�A�@�\)@�E�@��^@�/@��@��/@��`@�Ĝ@���@�j@��@��@���@���@�"�@�ȴ@���@�v�@�J@���@�p�@��@��@�I�@��P@��R@�^5@�5?@��@�hs@��@�V@��/@�z�@��u@��@�?}@��j@�bN@��@���@�ƨ@���@�\)@���@��R@��T@�@��^@�7L@��@�V@���@��9@�z�@�Z@�I�@�A�@�b@�;d@���@�~�@���@�hs@��@���@���@���@���@���@�A�@�|�@�C�@��@�ȴ@�ff@�=q@�=q@�{@��@�/@�V@��`@��@��D@�A�@��@�|�@�\)@�33@�ff@��@���@��h@�X@�7L@�V@��@��D@�j@�A�@��@��
@���@�S�@�@���@���@�v�@�$�@���@��-@�p�@�&�@��@�Ĝ@��@��u@�Z@�1@���@��m@��F@�|�@�33@��y@��R@���@��+@�~�@�n�@�M�@�@��-@��@��#@�x�@�?}@��9@�b@��
@��@�t�@�l�@�dZ@�C�@���@���@�1'@��F@z��@pĜ@f�+@^E�@T�@N�+@Fff@?+@4��@.5?@&�y@"��@ff@ �@��@&�@��@	��@5?11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   AԓuAԓuAԓuAԑhAԑhAԇ+AԓuAԝ�Aԝ�Aԝ�Aԡ�Aԡ�Aԣ�Aԡ�Aԛ�Aԝ�Aԝ�Aԝ�Aԡ�Aԥ�Aԥ�Aԥ�Aԣ�Aԡ�Aԡ�Aԏ\A�~�A�r�A�p�A�p�A�n�A�n�A�t�A�t�A�v�A�\)A���A��`A��`A���Ȁ\A�oA�/A�C�A���A��/A�~�A�l�A�
=A�ffA�ZA��;A�bNA�I�A��9A���A�K�A��HA�C�A�=qA���A��A��!A�  A�dZA�dZA�-A�l�A��HA��A�A���A��^A��\A��-A�&�A�I�A��A���A�M�A�ĜA��A�n�A�
=A��HA�/A���A�1A���A�C�A��mA�^5A�t�A�-A���A�bNA��A�;dA��A��A��7A���A��^A��A�z�A���A|�AxAv9XAs�#Ar�An$�Aj�AfJAb�!Aap�A_�hA]��A[�mAW�AS�wAQoAN�AL5?AI�hAGS�AB�yA>n�A<^5A;�hA:�uA8��A7�TA7l�A6��A6~�A5\)A3��A2I�A0��A/�mA.v�A-�7A-&�A,^5A+�A*��A)�
A(�`A'�A&�A%�^A$�DA#��A#�A#
=A"�HA"��A"JA!�hA bNA��A�\A��A��A�A33A�mA?}A �A��AO�AE�A�AƨA�hA;dA��A��AA�A��At�A�AVA	|�A�FA
=A��AbNA��AS�At�AZA+A �\@��
@��;@��R@��@��T@���@�1'@���@���@�G�@�;d@�5?@�r�@�j@�5?@�K�@��@��T@��#@�@柾@�-@��@��@��;@�Ĝ@�
=@�A�@�S�@��@���@ى7@���@�V@�r�@�Q�@���@�S�@���@���@ְ!@֟�@֏\@�ff@�V@�E�@��@Ձ@Դ9@�r�@�9X@�1'@��m@�\)@�33@��@�@�
=@�x�@�b@Ͼw@�o@Χ�@�p�@̓u@�\)@��@���@�C�@�
=@���@�=q@��@�?}@�%@���@ț�@���@���@�r�@��@���@ǥ�@�33@��@Ɵ�@�v�@�M�@�-@�@őh@�hs@�p�@�7L@�bN@���@�\)@���@�@�$�@��@�X@�V@��@��@��@�S�@��H@��\@�ff@�-@�@�G�@��`@��D@�(�@��@��H@�^5@���@�/@�&�@��/@��@���@� �@��w@��@�"�@��@���@��+@�5?@���@��@��m@��@��@�;d@�=q@��h@�/@���@�z�@�A�@�\)@�E�@��^@�/@��@��/@��`@�Ĝ@���@�j@��@��@���@���@�"�@�ȴ@���@�v�@�J@���@�p�@��@��@�I�@��P@��R@�^5@�5?@��@�hs@��@�V@��/@�z�@��u@��@�?}@��j@�bN@��@���@�ƨ@���@�\)@���@��R@��T@�@��^@�7L@��@�V@���@��9@�z�@�Z@�I�@�A�@�b@�;d@���@�~�@���@�hs@��@���@���@���@���@���@�A�@�|�@�C�@��@�ȴ@�ff@�=q@�=q@�{@��@�/@�V@��`@��@��D@�A�@��@�|�@�\)@�33@�ff@��@���@��h@�X@�7L@�V@��@��D@�j@�A�@��@��
@���@�S�@�@���@���@�v�@�$�@���@��-@�p�@�&�@��@�Ĝ@��@��u@�Z@�1@���@��m@��F@�|�@�33@��y@��R@���@��+@�~�@�n�@�M�@�@��-@��@��#@�x�@�?}@��9@�b@��
@��@�t�@�l�@�dZ@�C�@���G�O�@�1'@��F@z��@pĜ@f�+@^E�@T�@N�+@Fff@?+@4��@.5?@&�y@"��@ff@ �@��@&�@��@	��@5?11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�{B
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
��B�B1BaHB�^BȴB��B��B��B�B�wBƨBB�^B�LB�LB�9B�!B�B�B�B�!B�B��B��B��B��B��B��B�B�3B��B��B�%BjBiyBdZBYBQ�BB�B0!B)�BuB��B�B�B�`B��B�mB��B�'B�%BhsBaHBR�B+B\B
�B
��B
ŢB
�LB
�'B
��B
�uB
�%B
� B
q�B
^5B
D�B
�B	�B	�sB	�B	B	��B	�JB	o�B	gmB	k�B	aHB	[#B	XB	D�B	0!B	"�B	{B		7B��B�B�B�sB�B��B	B	{B	�B	�B	 �B	"�B	(�B	.B	6FB	A�B	E�B	K�B	O�B	P�B	Q�B	P�B	S�B	VB	ZB	o�B	w�B	{�B	�B	�B	�B	�1B	�7B	�1B	�B	~�B	x�B	v�B	u�B	t�B	hsB	VB	P�B	O�B	N�B	N�B	O�B	O�B	N�B	K�B	J�B	H�B	E�B	C�B	B�B	?}B	=qB	9XB	2-B	&�B	#�B	#�B	$�B	'�B	-B	0!B	2-B	/B	-B	,B	/B	2-B	=qB	E�B	D�B	I�B	J�B	M�B	N�B	bNB	l�B	iyB	e`B	`BB	W
B	O�B	H�B	E�B	N�B	^5B	YB	W
B	ZB	\)B	_;B	ffB	dZB	o�B	m�B	v�B	}�B	|�B	|�B	� B	�VB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�3B	�3B	�-B	�3B	�9B	�FB	�FB	�LB	�XB	ĜB	ȴB	ɺB	ȴB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�#B	�B	�B	�B	�#B	�)B	�5B	�/B	�/B	�5B	�;B	�BB	�HB	�HB	�HB	�HB	�HB	�HB	�5B	�/B	�/B	�)B	�)B	�)B	�/B	�5B	�5B	�5B	�;B	�;B	�BB	�BB	�;B	�BB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�TB	�ZB	�ZB	�TB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
1B
	7B
	7B
1B
1B
1B
1B
	7B
	7B
	7B

=B
PB
PB
PB
PB
PB
JB
JB
JB
PB
PB
VB
VB
VB
VB
\B
\B
VB
VB
VB
VB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
bB
hB
hB
hB
hB
oB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&�B
�B
�B
#�B
,B
2-B
9XB
=qB
E�B
K�B
P�B
YB
^5B
e`B
iyB
m�B
r�B
v�B
y�B
|�B
� B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
�[B
�cB
�cB
�cB
�cB
�cB
�aB
�`B
�bB
�dB
�bB
�bB
�bB
�bB
�dB
�dB
�dB
�bB
�dB
�dB
�bB
�bB
�bB
�bB
�bB
�iB
�mB
�rB
�sB
�}B
�}B
�B
��B
��B
��B
��B
ʤB
��B��BBa!B�<BȑB�{B��B��B��B�SBƅB�jB�9B�(B�'B�B��B��B��B��B��B��B��B��B��B��B�_B��B��B�B��B��B��BjXBiNBd3BX�BQ�BBfB/�B)�BKB��B�hB�[B�5B��B�>B̤B��B��BhJBa BR�B*�B3B
�B
��B
�yB
�%B
��B
��B
�KB
��B
�B
q�B
^B
DsB
YB	�B	�LB	��B	�lB	��B	�'B	o~B	gIB	kbB	a'B	[B	W�B	D|B	0 B	"�B	]B		B��B�B�aB�UB�B��B	�B	XB	jB	�B	 �B	"�B	(�B	-�B	6!B	AeB	E}B	K�B	O�B	P�B	Q�B	P�B	S�B	U�B	Y�B	owB	w�B	{�B	��B	��B	��B	�B	�B	�	B	��B	~�B	x�B	v�B	u�B	t�B	hNB	U�B	P�B	O�B	N�B	N�B	O�B	O�B	N�B	K�B	J�B	H�B	EzB	ClB	BgB	?VB	=GB	90B	2B	&�B	#�B	#�B	$�B	'�B	,�B	/�B	2B	.�B	,�B	+�B	.�B	2B	=HB	E{B	DsB	I�B	J�B	M�B	N�B	b#B	lbB	iOB	e5B	`B	V�B	O�B	H�B	EyB	N�B	^B	X�B	V�B	Y�B	[�B	_B	f8B	d0B	orB	mdB	v�B	}�B	|�B	|�B	�B	�*B	�9B	�@B	�HB	�OB	�SB	�SB	�XB	�XB	�YB	�YB	�`B	�`B	�fB	�lB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�	B	�B	�B	�B	�'B	�mB	ȄB	ɍB	ȅB	ǀB	�~B	ɊB	˖B	̝B	ͥB	ϯB	зB	дB	ѻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�	B	�
B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�"B	�%B	�)B	�*B	�$B	�!B	�!B	�(B	�.B	�3B	�<B	�<B	�;B	�:B	�BB	�GB	�GB	�FB	�GB	�KB	�eB	�lB	�kB	�qB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	B
	B
	B
	B
	B
	B
�B
	B
	B
�B
�B
�B
�B
	B
	B
	B


B
B
B
B
B
B
B
B
B
B
B
!B
"B
$B
"B
)B
)B
"B
 B
!B
"B
"B
#B
'B
'B
)B
(B
/B
0B
1B
0B
/B
4B
4B
4B
3B
<B
:B
7B
ZB
ZB
XB
YB
SB
LB
LB
KB
LB
NB
MB
MB
TG�O�B
^B
~B
#�B
+�B
1�B
9#B
=;B
EnB
K�B
P�B
X�B
^B
e+B
iEB
m\B
r{B
v�B
y�B
|�B
�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.44 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451242016080714512420160807145124  AO  ARCAADJP                                                                    20151222102058    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151222102058  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151222102058  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145124  IP                  G�O�G�O�G�O�                