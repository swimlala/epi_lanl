CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-11-24T03:15:27Z AOML 3.0 creation; 2016-05-31T19:14:46Z UW 3.1 conversion     
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
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20151124031527  20160531121446  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051_7090_131                   2C  D   APEX                            5368                            041511                          846 @׀��=�&1   @׀��FG_@3���n��da��l�D1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBxffB33B�  B�  B�ffB���B�  B���B���B�  B�  B�  B�33B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D� D�C3D�vfD�� D���D�Y�D�� D��fD�	�D�I�D�|�D���D�fD�9�DچfD��D�  D�6fD��D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBxffB33B�  B�  B�ffB���B�  B���B���B�  B�  B�  B�33B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D� D�C3D�vfD�� D���D�Y�D�� D��fD�	�D�I�D�|�D���D�fD�9�DچfD��D�  D�6fD��D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A���A��A���AټjAٸRAٶFAٶFAٶFAٴ9Aٲ-Aٲ-Aٲ-AٮA٣�Aه+A�x�A�v�A�r�A�n�A�jA�`BA�XA�O�A�S�A�XA�XA�XA�^5A�dZAكA٬A�bA��A�JA��;A��A�9XAőhAć+A��A���A£�A�Q�A��`A� �A���A��yA�;dA��A�(�A��7A�7LA�z�A��7A���A��A�jA�ȴA��FA�XA��#A�
=A���A���A�r�A�G�A��A� �A�r�A��A��TA���A���A��\A��PA�A��jA��FA��RA��TA�M�A��A��PA��-A�jA�ffA��TA�n�A�z�A��yA�l�A��A�JA��#A��
A�(�A���A�|�A�G�A���A�/A~E�A{ƨAz��Ay�Aw��AuAt��As?}Aqt�Ap(�Ao��AoAl�Aj �Ah�!Af�uAe��Ad�uAb(�A]�AZ=qAW&�AT��AQ��AO�PAN�RAM��AM33AK7LAJ�DAI7LAG��AF1AD�AC�hABZA@��A?��A=p�A;��A;XA:=qA9A85?A6�A5��A4^5A4A2�A2�A1��A17LA0�+A/�A.n�A-�FA,n�A+G�A*I�A)`BA((�A'�-A&��A%ƨA$z�A#��A"�!A!��A   A�mA~�A1A�RA5?A�mA��AdZA
=A-A�HAbA�/A�A�7A|�A��A�mAz�AVA~�AA��A�A	�A	%A�\A��A��A1'A�jA�mAC�A ffA $�A �@��@��+@��@�$�@���@��@��@�O�@��m@�\)@��@�J@�r�@���@�$�@���@�Z@�ff@��@��@�@�G�@��@ߕ�@ޏ\@݉7@��@۾w@�33@�J@؋D@�b@�+@ղ-@�G�@���@��#@���@�1'@�n�@·+@�n�@Ͳ-@��/@�l�@�n�@�&�@�r�@� �@��;@�;d@ƸR@�{@őh@��/@�+@ÍP@Õ�@�@���@�&�@��u@���@���@��@�%@�z�@��9@���@���@��D@�t�@�ƨ@��@�\)@�=q@�{@���@�X@���@�I�@��w@�dZ@�ff@���@�7L@�1@�|�@�"�@���@��\@�V@�`B@��@���@���@���@���@���@�ff@���@��@�V@��u@�(�@��m@���@�S�@���@�V@��T@��h@�G�@��@��9@��D@��m@���@�dZ@�33@�o@��@�n�@���@���@���@���@�x�@�X@�V@���@��j@� �@��;@��@�S�@�"�@�
=@��y@���@�ff@�5?@�@��#@���@��@�p�@�G�@�/@��@�%@���@�bN@���@��F@��P@�;d@��@�^5@���@���@�hs@�G�@��@��/@�z�@���@�ƨ@��F@��P@�\)@�@��!@���@�~�@�V@�{@��T@��h@�X@�O�@�&�@��`@���@�z�@���@�\)@�dZ@�C�@�n�@��-@���@��^@���@���@�@�O�@��@���@�Z@�1@�t�@�v�@���@��\@���@��!@�V@�x�@� �@���@�9X@�/@��@��`@�1'@�r�@�?}@�/@�%@��`@��9@��m@�dZ@�A�@�9X@��m@�l�@��@��\@��y@�ȴ@���@��+@�n�@���@��@�33@��@��y@��@�\)@���@�E�@���@��#@��\@��7@��@��@��@�=q@�ff@�-@��T@�p�@���@�%@��@��@��/@���@�Q�@���@�;d@�S�@�@���@���@���@�M�@�n�@�M�@�M�@�$�@�J@���@��-@�x�@�hs@��@��@���@}@w��@ol�@f��@\��@Q��@J^5@D(�@<1@3��@-?}@(b@%/@   @@�;@�@��@V@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A�A���A��A���AټjAٸRAٶFAٶFAٶFAٴ9Aٲ-Aٲ-Aٲ-AٮA٣�Aه+A�x�A�v�A�r�A�n�A�jA�`BA�XA�O�A�S�A�XA�XA�XA�^5A�dZAكA٬A�bA��A�JA��;A��A�9XAőhAć+A��A���A£�A�Q�A��`A� �A���A��yA�;dA��A�(�A��7A�7LA�z�A��7A���A��A�jA�ȴA��FA�XA��#A�
=A���A���A�r�A�G�A��A� �A�r�A��A��TA���A���A��\A��PA�A��jA��FA��RA��TA�M�A��A��PA��-A�jA�ffA��TA�n�A�z�A��yA�l�A��A�JA��#A��
A�(�A���A�|�A�G�A���A�/A~E�A{ƨAz��Ay�Aw��AuAt��As?}Aqt�Ap(�Ao��AoAl�Aj �Ah�!Af�uAe��Ad�uAb(�A]�AZ=qAW&�AT��AQ��AO�PAN�RAM��AM33AK7LAJ�DAI7LAG��AF1AD�AC�hABZA@��A?��A=p�A;��A;XA:=qA9A85?A6�A5��A4^5A4A2�A2�A1��A17LA0�+A/�A.n�A-�FA,n�A+G�A*I�A)`BA((�A'�-A&��A%ƨA$z�A#��A"�!A!��A   A�mA~�A1A�RA5?A�mA��AdZA
=A-A�HAbA�/A�A�7A|�A��A�mAz�AVA~�AA��A�A	�A	%A�\A��A��A1'A�jA�mAC�A ffA $�A �@��@��+@��@�$�@���@��@��@�O�@��m@�\)@��@�J@�r�@���@�$�@���@�Z@�ff@��@��@�@�G�@��@ߕ�@ޏ\@݉7@��@۾w@�33@�J@؋D@�b@�+@ղ-@�G�@���@��#@���@�1'@�n�@·+@�n�@Ͳ-@��/@�l�@�n�@�&�@�r�@� �@��;@�;d@ƸR@�{@őh@��/@�+@ÍP@Õ�@�@���@�&�@��u@���@���@��@�%@�z�@��9@���@���@��D@�t�@�ƨ@��@�\)@�=q@�{@���@�X@���@�I�@��w@�dZ@�ff@���@�7L@�1@�|�@�"�@���@��\@�V@�`B@��@���@���@���@���@���@�ff@���@��@�V@��u@�(�@��m@���@�S�@���@�V@��T@��h@�G�@��@��9@��D@��m@���@�dZ@�33@�o@��@�n�@���@���@���@���@�x�@�X@�V@���@��j@� �@��;@��@�S�@�"�@�
=@��y@���@�ff@�5?@�@��#@���@��@�p�@�G�@�/@��@�%@���@�bN@���@��F@��P@�;d@��@�^5@���@���@�hs@�G�@��@��/@�z�@���@�ƨ@��F@��P@�\)@�@��!@���@�~�@�V@�{@��T@��h@�X@�O�@�&�@��`@���@�z�@���@�\)@�dZ@�C�@�n�@��-@���@��^@���@���@�@�O�@��@���@�Z@�1@�t�@�v�@���@��\@���@��!@�V@�x�@� �@���@�9X@�/@��@��`@�1'@�r�@�?}@�/@�%@��`@��9@��m@�dZ@�A�@�9X@��m@�l�@��@��\@��y@�ȴ@���@��+@�n�@���@��@�33@��@��y@��@�\)@���@�E�@���@��#@��\@��7@��@��@��@�=q@�ff@�-@��T@�p�@���@�%@��@��@��/@���@�Q�@���@�;d@�S�@�@���@���@���@�M�@�n�@�M�@�M�@�$�@�J@���@��-@�x�@�hs@��@��@���@}@w��@ol�@f��@\��@Q��@J^5@D(�@<1@3��@-?}@(b@%/@   @@�;@�@��@V@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�{B�uB�uB�oB�oB�hB�hB�oB�hB�hB�oB�oB�oB�oB�uB��B��B��B��B��B��B��B��B��B�BĜB��B��B��B�B�ZB
=B_;B�wB�B>wB[#BT�BP�BcTBiyBl�BiyBgmBaHBXB^5B]/BYBXBP�BE�B9XB6FB1'B(�B�BVBB��B�B�BB��B��B�'B��B�PBgmB=qBbB��B�HBƨB�wB�B�Bt�Bo�Bq�Bt�Bw�B�=B�!B�B��B��B�B�}B��B�bB�Bs�BZB>wB1'B�B  B
�B
�)B
ÖB
�3B
�B
��B
�PB
|�B
p�B
iyB
T�B
D�B
:^B
-B
�B
oB
VB
1B	�B	�HB	�B	ĜB	�^B	�-B	��B	}�B	hsB	T�B	C�B	33B	(�B	$�B	 �B	�B	{B	bB	
=B	B��B��B�B�B�ZB�;B�)B�B��B��B��BɺBɺBƨBB�}B�jB�XB�LB�FB�9B�'B�B�B��B��B��B��B��B��B��B��B��B�uB�bB�PB�=B�B�B�B�B�B�1B�+B�%B�+B�+B�+B�B�%B�7B�=B�1B�1B�+B�+B�B�B�B�B}�B|�B{�By�Bw�Bv�Bt�Br�Br�Bs�Bt�Bu�Bt�Bs�Bs�Bs�Bt�Bw�Bx�Bz�Bz�B|�B}�B|�Bz�Bx�Bw�Bz�B}�B|�B{�B{�By�By�Bz�Bw�Bu�Bw�By�B|�B|�B|�B}�B� B� B�B�B�B�B�%B�%B�%B�7B�bB��B��B��B��B�{B�hB�hB�oB�uB��B��B��B��B��B��B��B�B��B��B��B�B�'B�'B�-B�FB�}BȴB��B�B�B�;B�mB�yB�B�B�B��B��B��B��B	  B	B	B	B	DB	{B	�B	�B	�B	�B	�B	$�B	'�B	(�B	/B	5?B	8RB	<jB	@�B	E�B	I�B	L�B	O�B	Q�B	T�B	XB	ZB	\)B	_;B	aHB	cTB	e`B	gmB	iyB	iyB	l�B	n�B	o�B	p�B	p�B	q�B	u�B	z�B	{�B	{�B	|�B	~�B	�B	�B	�B	�B	�+B	�+B	�+B	�1B	�7B	�7B	�=B	�JB	�VB	�\B	�hB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�?B	�FB	�FB	�LB	�RB	�dB	�dB	�jB	�qB	�wB	�}B	��B	B	ÖB	ÖB	ĜB	ŢB	ŢB	ǮB	ƨB	ƨB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�BB	�HB	�;B	�/B	�HB	�sB	�yB	�yB	�yB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
  B
B
%B
1B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
bB
�B
�B
#�B
+B
49B
7LB
@�B
G�B
L�B
Q�B
YB
_;B
dZB
hsB
m�B
o�B
r�B
v�B
y�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�|B�wB�tB�pB�pB�hB�hB�nB�hB�hB�pB�oB�nB�oB�rB��B��B��B��B��B��B��B��B��B�	BġB��B��B��B�B�WB
;B_?B�yB�B>{B[&BUBP�BcXBiBl�BiBgrBaLBXB^6B]2BYBXBP�BE�B9]B6GB10B(�B�BWB	B��B�B�EB��B��B�'B��B�NBgmB=sBbB��B�JBƧB�wB�B�#Bt�Bo�Bq�Bt�Bw�B�?B�!B�B��B��B�B�~B��B�`B�Bs�BZB>xB1+B�B B
�B
�)B
ÕB
�7B
�B
��B
�UB
|�B
p�B
iB
UB
D�B
:dB
-B
�B
tB
^B
9B	�B	�OB	�
B	ĦB	�hB	�8B	��B	~B	h�B	UB	C�B	3BB	)B	$�B	 �B	�B	�B	rB	
LB	+B��B��B�B�B�lB�LB�<B�"B�
B��B��B��B��BƹB¢B��B�}B�jB�^B�ZB�MB�<B�/B�B�B�B��B��B��B��B��B��B��B��B�yB�fB�SB�5B�)B�(B�B�3B�HB�BB�9B�BB�AB�BB�3B�;B�NB�SB�GB�HB�CB�AB�5B�(B�"B�B~B}B{�By�Bw�Bv�Bt�Br�Br�Bs�Bt�Bu�Bt�Bs�Bs�Bs�Bt�Bw�Bx�Bz�Bz�B}B~B}Bz�Bx�Bw�Bz�B~B}B{�B{�By�By�Bz�Bw�Bu�Bw�By�B}B}B}B~
B�B�B� B�0B�'B�3B�;B�8B�9B�KB�uB��B��B��B��B��B�}B�|B��B��B��B��B��B��B��B��B��B�B�B�
B�	B�B�;B�9B�@B�YB��B��B��B�B�-B�LB�}B�B�B�B�B��B��B��B� B	 B	B	!B	-B	QB	�B	�B	�B	�B	�B	�B	$�B	'�B	)B	/)B	5MB	8^B	<xB	@�B	E�B	I�B	L�B	O�B	Q�B	U
B	XB	Z(B	\5B	_HB	aUB	c_B	elB	gxB	i�B	i�B	l�B	n�B	o�B	p�B	p�B	q�B	u�B	z�B	{�B	{�B	|�B	B	�B	�B	�"B	�(B	�8B	�5B	�5B	�:B	�?B	�@B	�HB	�UB	�`B	�gB	�qB	�zB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�
B	�B	�B	�B	�$B	�,B	�1B	�HB	�NB	�NB	�VB	�ZB	�mB	�mB	�rB	�yB	�B	��B	��B	B	ÝB	ÝB	ĤB	ũB	ŪB	ǶB	ƯB	ưB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�HB	�NB	�BB	�5B	�QB	�yB	�B	�B	�B	�zB	�{B	�xB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	� B	��B	��B	�B
B
B
B
B
B
B
B
B
B
B
B
 B
B
+B
7B

EB

@B

CB

CB

DB

BB
JB
LB
KB
KB
jB
�B
�B
#�B
+B
4@B
7PB
@�B
G�B
L�B
Q�B
YB
_?B
d]B
hwB
m�B
o�B
r�B
v�B
y�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214462016053112144620160531121446  AO  ARCAADJP                                                                    20151124031527    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151124031527  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151124031527  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121446  IP                  G�O�G�O�G�O�                