CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-06T19:17:50Z AOML 3.0 creation; 2016-08-07T21:51:19Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150706191750  20160807145119  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               >A   AO  5287_9017_062                   2C  D   APEX                            6529                            072314                          846 @�]���1   @�]ڙ��@0�E�����d�7KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    >A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�ffB�  B���B���B���B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�ffBי�B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCM�fCO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy� D� D�<�D��fD���D��D�<�D�s3D�ٚD��D�)�D��fD��3D�3D�<�Dڜ�D��3D�3D�33D�p D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@�{A
=A'
=AG
=Ag
=A��A��A��A��AÅAӅA�A�B\)B	BBB!B)B1B9BABIBQBYBaBiBqByB��HB��HB��HB��HB��HB��HB�G�B��HB��B��B��B��HB��HB�{B��HB��HB��HBĮB��HB��HB��HB�G�B�z�B��HB��HB��HB��HB��HB��HB��HB��HB��HC p�Cp�Cp�Cp�Cp�C
p�Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�C p�C"p�C$p�C&p�C(p�C*p�C,p�C.p�C0p�C2p�C4p�C6�>C8p�C:p�C<p�C>p�C@p�CBp�CDp�CFp�CHp�CJp�CLW
CNW
CPW
CRp�CTp�CVp�CXp�CZp�C\p�C^p�C`p�Cb�>Cdp�Cf�>Chp�Cjp�Clp�Cnp�Cpp�Crp�Ctp�Cvp�Cxp�Czp�C|p�C~p�C�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�EC�8RC�EC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!)D!�)D")D"�)D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dt�)Dy�)D�D�J�D��zD���D��D�J�D��GD��D���D�7�D��zD��GD�!GD�J�Dڪ�D��GD�GD�AGD�~D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�C�A�A�A�?}A�A�A�E�A�K�A�I�A�G�A�G�A�E�A�bA��A�=qA�ffA�^5A��A���Aܺ^Aܙ�A�bNA��A��A��
A�ƨAە�A�v�A�r�A�p�A�n�A�VA�33A���Aڧ�AڅA�hsA�hsA�O�A�9XA���A�ZAؓuAՃA�oA�ƨAӇ+A�dZA��;A�l�A�C�A�Q�A��A˲-A�1'A��A�C�A���A�VAƏ\A���A���A��;A��A�ZA���A�ƨA���A�&�A�C�A�O�A���A�7LA�C�A�ĜA�=qA�ȴA��A�~�A�(�A�x�A�;dA�t�A�  A�XA��A���A�(�A�x�A��!A��7A�z�A�A�/A�x�A�VA�C�A�ffA�hsA��+A�-A�5?A�JA�K�A���A���A��A�hsA��A��\A�A�A�%A�(�A��!A��;A��
A}�Ax1Au�
AqdZAm"�AhVA`v�A\(�AY�mAV��AT�RASXARE�AO�7AL=qAJ�AG|�AB=qA?VA<9XA9�-A8v�A5��A29XA/hsA-�hA,��A,A+�A)�
A(��A(JA&��A$��A#A#t�A"��A!�-A 1Ap�AK�A%A�;AoA�A�FAO�A�A  A�FA\)Av�A��A�^AE�A�yA�TA��A"�A
��A
I�A	�
A��A^5At�A��AO�AVA�A�!A�PA�mAv�A�9A��A�A �A =q@���@���@��@�S�@�%@��w@�\)@�@��@�+@�$�@�G�@��@�1'@�@��@�ȴ@�=q@���@�/@��@�X@�-@�@�(�@睲@�K�@�V@��T@��`@��;@�33@�o@�E�@��/@�dZ@�E�@��@��@�`B@�A�@�o@��T@�hs@��@�1'@��@�bN@�9X@�ƨ@�
=@�{@�p�@ԃ@��m@��@�v�@Ѳ-@���@�z�@Ѓ@�1'@϶F@�o@θR@�ff@�~�@Ο�@Ο�@Η�@�=q@��#@��@̃@�r�@�bN@˶F@�@ʇ+@�M�@�=q@�^5@�$�@���@�@�p�@��@� �@��;@Ǿw@ǍP@�|�@�|�@�@�{@�O�@�V@Ĵ9@�
=@+@��y@�@�+@�ff@���@�1@�r�@���@��@�X@��@�Q�@��@��
@���@�|�@���@�v�@�M�@���@�$�@�J@��@�G�@��@�-@�%@�bN@�  @��F@��P@��F@�l�@�@�=q@�J@�{@��@�^5@�~�@��@��T@���@��7@�p�@�G�@��@�O�@�O�@���@�j@�1@��m@�|�@�K�@�o@�ȴ@��+@�5?@�@�O�@�Ĝ@��9@���@��9@��@���@��D@�1'@�  @��@��R@�V@�-@��T@��#@��T@��T@��@��T@��-@�G�@���@�Ĝ@��u@�Z@��@���@�l�@�K�@�K�@���@�$�@��@�{@��#@�Ĝ@���@���@��9@��@�z�@�1'@��@��m@��;@��
@���@�"�@��@���@�v�@�V@�-@�{@��T@���@���@��7@�V@���@��@�1'@�ƨ@��@��@�~�@�E�@�5?@���@��h@�p�@�G�@�7L@���@��`@��@�bN@�  @��@�\)@��@�v�@�5?@��T@���@��@�hs@�/@��@���@�ƨ@��w@��@��P@�l�@��H@�5?@��@���@��7@�V@�Z@�1'@�  @�ƨ@�S�@�ȴ@���@�M�@��@���@�@���@��@�X@��@���@��@�A�@� �@�  @�t�@�S�@�C�@�
=@��H@��R@�V@��@��@��-@�`B@�%@��u@�V@��@�hs@z��@st�@k�
@co@Z^5@N��@FE�@>�+@:J@2^5@+��@%�@!%@j@�@��@�+@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�C�A�A�A�?}A�A�A�E�A�K�A�I�A�G�A�G�A�E�A�bA��A�=qA�ffA�^5A��A���Aܺ^Aܙ�A�bNA��A��A��
A�ƨAە�A�v�A�r�A�p�A�n�A�VA�33A���Aڧ�AڅA�hsA�hsA�O�A�9XA���A�ZAؓuAՃA�oA�ƨAӇ+A�dZA��;A�l�A�C�A�Q�A��A˲-A�1'A��A�C�A���A�VAƏ\A���A���A��;A��A�ZA���A�ƨA���A�&�A�C�A�O�A���A�7LA�C�A�ĜA�=qA�ȴA��A�~�A�(�A�x�A�;dA�t�A�  A�XA��A���A�(�A�x�A��!A��7A�z�A�A�/A�x�A�VA�C�A�ffA�hsA��+A�-A�5?A�JA�K�A���A���A��A�hsA��A��\A�A�A�%A�(�A��!A��;A��
A}�Ax1Au�
AqdZAm"�AhVA`v�A\(�AY�mAV��AT�RASXARE�AO�7AL=qAJ�AG|�AB=qA?VA<9XA9�-A8v�A5��A29XA/hsA-�hA,��A,A+�A)�
A(��A(JA&��A$��A#A#t�A"��A!�-A 1Ap�AK�A%A�;AoA�A�FAO�A�A  A�FA\)Av�A��A�^AE�A�yA�TA��A"�A
��A
I�A	�
A��A^5At�A��AO�AVA�A�!A�PA�mAv�A�9A��A�A �A =q@���@���@��@�S�@�%@��w@�\)@�@��@�+@�$�@�G�@��@�1'@�@��@�ȴ@�=q@���@�/@��@�X@�-@�@�(�@睲@�K�@�V@��T@��`@��;@�33@�o@�E�@��/@�dZ@�E�@��@��@�`B@�A�@�o@��T@�hs@��@�1'@��@�bN@�9X@�ƨ@�
=@�{@�p�@ԃ@��m@��@�v�@Ѳ-@���@�z�@Ѓ@�1'@϶F@�o@θR@�ff@�~�@Ο�@Ο�@Η�@�=q@��#@��@̃@�r�@�bN@˶F@�@ʇ+@�M�@�=q@�^5@�$�@���@�@�p�@��@� �@��;@Ǿw@ǍP@�|�@�|�@�@�{@�O�@�V@Ĵ9@�
=@+@��y@�@�+@�ff@���@�1@�r�@���@��@�X@��@�Q�@��@��
@���@�|�@���@�v�@�M�@���@�$�@�J@��@�G�@��@�-@�%@�bN@�  @��F@��P@��F@�l�@�@�=q@�J@�{@��@�^5@�~�@��@��T@���@��7@�p�@�G�@��@�O�@�O�@���@�j@�1@��m@�|�@�K�@�o@�ȴ@��+@�5?@�@�O�@�Ĝ@��9@���@��9@��@���@��D@�1'@�  @��@��R@�V@�-@��T@��#@��T@��T@��@��T@��-@�G�@���@�Ĝ@��u@�Z@��@���@�l�@�K�@�K�@���@�$�@��@�{@��#@�Ĝ@���@���@��9@��@�z�@�1'@��@��m@��;@��
@���@�"�@��@���@�v�@�V@�-@�{@��T@���@���@��7@�V@���@��@�1'@�ƨ@��@��@�~�@�E�@�5?@���@��h@�p�@�G�@�7L@���@��`@��@�bN@�  @��@�\)@��@�v�@�5?@��T@���@��@�hs@�/@��@���@�ƨ@��w@��@��P@�l�@��H@�5?@��@���@��7@�V@�Z@�1'@�  @�ƨ@�S�@�ȴ@���@�M�@��@���@�@���@��@�X@��@���@��@�A�@� �@�  @�t�@�S�@�C�@�
=@��H@��R@�V@��@��@��-@�`B@�%G�O�@�V@��@�hs@z��@st�@k�
@co@Z^5@N��@FE�@>�+@:J@2^5@+��@%�@!%@j@�@��@�+@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	E�B	E�B	F�B	F�B	E�B	F�B	F�B	F�B	F�B	F�B	K�B	^5B	YB	gmB	iyB	gmB	l�B	u�B	y�B	x�B	w�B	w�B	w�B	v�B	r�B	o�B	n�B	o�B	p�B	s�B	v�B	y�B	x�B	}�B	�%B	�1B	�oB	�B	�'B	�XB	ƨB
\B
XB
�PB
�/BO�Be`BffB�^B��B��B%B�BQ�BbNBhsBu�B��B�B��B��B�+By�Bq�BgmBYBG�BB�B;dB9XB8RB7LB7LB6FB6FB5?B49B2-B/B%�B�B�B\BB��B��B�fB�5B��BɺBǮB��B�B��B�VBw�B`BBH�B(�B+B
��B
�)B
ɺB
�9B
��B
��B
��B
�hB
�1B
w�B
gmB
[#B
R�B
D�B
�B	�B	��B	�XB	��B	�%B	jB	H�B	2-B	$�B	�B	oB	DB	B��B��B�B�`B��B��B��B��B��B��BǮBŢBƨBɺB��B�B�B�B�B�B�5B�)B�#B�B�B�;B�)B�B�/B�B�fB�B�B�B��B��B	B	B	%B	B�B�;B�B��B��B�}B�jB�RB�?B�?B�FB�?B�FB�RB�}BĜB��B�B�BB�yB�B�B�mB�;B�B�B�B�B�B�mB�mB�mB�fB�mB�yB�B�B�B�B�B�B�B�B�B�mB�B�B��B��B��B�B��B��B��B��B��B	B	B	B��B��B��B��B��B	%B	
=B	1B	1B		7B	bB	�B	�B	'�B	,B	.B	1'B	49B	<jB	E�B	K�B	K�B	P�B	R�B	T�B	W
B	XB	YB	\)B	^5B	_;B	bNB	dZB	e`B	ffB	ffB	gmB	gmB	hsB	iyB	iyB	iyB	jB	m�B	q�B	r�B	t�B	x�B	� B	�B	�B	�B	�+B	�VB	�\B	�\B	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�?B	�?B	�FB	�FB	�LB	�RB	�dB	�jB	�qB	�dB	�XB	�3B	�'B	�B	�B	�B	�B	�B	�-B	�-B	�-B	�3B	�9B	�?B	�RB	�^B	�dB	�dB	�jB	�qB	�}B	��B	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�5B	�/B	�5B	�;B	�BB	�BB	�HB	�NB	�ZB	�`B	�mB	�mB	�sB	�sB	�yB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
+B
1B
1B
1B
	7B

=B
\B
�B
%�B
,B
/B
49B
;dB
@�B
G�B
M�B
S�B
W
B
]/B
aHB
ffB
iyB
n�B
r�B
u�B
z�B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	E�B	E�B	F�B	F�B	E�B	F�B	F�B	F�B	F�B	F�B	K�B	^"B	YB	gYB	igB	g[B	lxB	u�B	y�B	x�B	w�B	w�B	w�B	v�B	r�B	o�B	n�B	o�B	p�B	s�B	v�B	y�B	x�B	}�B	�B	�B	�YB	��B	�B	�=B	ƐB
CB
W�B
�2B
�BO�Be?BfDB�=B��B��B B\BQ�Bb*BhRBu�B�{B��B��B��B�By�Bq�BgFBX�BG�BBiB;@B91B8.B7(B7%B6#B6"B5B4B2B.�B%�BmB\B5B�B��B��B�@B�B��BɑBǇB�ZB��B��B�-Bw�B`BH�B(�BB
��B
�B
ɒB
�B
��B
��B
�lB
�CB
�	B
w�B
gDB
Z�B
R�B
DuB
pB	�eB	ʟB	�7B	��B	�B	j^B	H�B	2B	$�B	�B	PB	&B	�B��B��B�sB�AB��B̮B˩BʹBθBʣBǎBłBƈBɚB˨B��B��B��B��B��B�B�B�B��B��B�B�B��B�B��B�CB�uB�B�B��B��B	�B	�B	 B	�B��B�B��B��BʞB�[B�GB�.B�B�B�%B�B�%B�/B�WB�yB��B��B�B�UB�B��B�HB�B�yB�lB�`B�jB�]B�IB�FB�GB�?B�EB�TB�^B�iB�xB�wB�B�B�B�}B�eB�GB�ZB�qB��B��B��B�B��B��B��B��B��B	 �B	 �B	 �B��B��B��B��B��B	�B	
B		B	B		B	9B	_B	�B	'�B	+�B	-�B	0�B	4B	<>B	ExB	K�B	K�B	P�B	R�B	T�B	V�B	W�B	X�B	[�B	^	B	_B	b!B	d,B	e2B	f9B	f9B	gAB	gCB	hIB	iKB	iMB	iMB	jQB	mbB	q{B	r�B	t�B	x�B	�B	��B	��B	��B	��B	�(B	�.B	�/B	�/B	�NB	�eB	�vB	�qB	�cB	�^B	�VB	�QB	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�6B	�<B	�AB	�5B	�'B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�.B	�6B	�4B	�9B	�BB	�JB	�SB	�lB	�vB	ȃB	ʑB	ʐB	˕B	̜B	͡B	͢B	ΧB	жB	гB	��B	��B	ѻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�	B	�B	�B	�B	�B	�)B	�.B	�:B	�<B	�BB	�AB	�GB	�CB	�CB	�CB	�DB	�CB	�BB	�AB	�HB	�KB	�SB	�YB	�ZB	�aB	�aB	�_B	�eB	�gB	�gB	�lB	�rB	�qB	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
�B
�B
	G�O�B
)B
lB
%�B
+�B
.�B
4B
;2B
@PB
GzB
M�B
S�B
V�B
\�B
aB
f2B
iAB
nbB
r|B
u�B
z�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.44 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451192016080714511920160807145119  AO  ARCAADJP                                                                    20150706191750    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150706191750  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150706191750  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145119  IP                  G�O�G�O�G�O�                