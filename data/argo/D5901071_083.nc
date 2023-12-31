CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:14Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  e<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g$   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �D   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �t   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               SA   AO  20111130141030  20190522121826  1727_5046_083                   2C  D   APEX                            2143                            040306                          846 @Ԁ��@1   @Ԁ﬐ @8KC��%�dXbM�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  Dy�D��D� D   D � D!  D!� D"  D"y�D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DBy�DB��DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� Dd��De� Df  Df� Df��Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�ff@�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  Dy�D��D� D   D � D!  D!� D"  D"y�D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DBy�DB��DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� Dd��De� Df  Df� Df��Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�z�A�x�A�r�A�t�A�x�A�t�ÁA̅A̅Ȧ+Ȧ+Ȧ+Ȁ\A̓uA̓uA̅ÃÃA̅ÃA̅A̅Ả7A���A�oA�^5A˅A�-A��A�ȴAŃA�A�&�A��A��A��/AhA��A��^A�(�A�VA���A�K�A�(�A�`BA�A�oA��uA���A��jA���A�ffA��wA��uA�C�A�{A���A�dZA�{A��^A��A�VA��-A�33A��mA�ffA�&�A��mA��hA���A�XA���A�C�A�z�A��
A�"�A�?}A���A�7LA�S�A�oA�ȴA�+A��A��+A���A�ffA�Q�A��HA���A��A�C�A���A��A�jA��+A��yA��A�E�A���A��A��A��!A��RA��`A���A���A�ffA�I�A�ĜA�1'A�M�A��+A���A��A�x�A��yA��A��FA�$�A��/A���A�bA��A�1A}��A|-Azn�AxZAu�At1'Ar-ApbAn��AnbNAm`BAlVAk�hAkx�Aj�DAg�Af�AfbAe�Acl�Ab^5Aa�
Aa�^Aa?}A`E�A_�mA^ĜA\��AZ��AX��AW\)AV(�AUl�AT�AT��AT9XAS��AR�AQ�#AQK�AP�AO��AL��AKhsAJI�AHQ�AF�AE��AE33AC�mA@A�A>jA=|�A=K�A<�yA;�FA;?}A:��A:��A:��A9�-A8E�A7�hA7�A6-A5|�A4(�A3+A2�\A1�-A0��A0ffA/�hA.ĜA.jA-��A,��A+`BA*  A)�FA)�PA(�RA'�mA&��A%C�A$��A$1A"��A"(�A!�hA!VA �/A �\A A��A7LAQ�A\)A�9A�-A�uA�mAdZAbAC�AĜAbNA�`A=qA��A�jA�AZA��A�Ap�AA9XA��AA
ĜA
$�A�HA�A�A-A�HAA�AƨA?}A��A^5A �/@�ƨ@���@�{@���@���@�l�@�{@��u@�I�@��P@��@�A�@��@��#@�9@�@��@�{@�"�@�j@�=q@�=q@��@ۥ�@�=q@ّh@ش9@��
@�ƨ@�
=@�{@�/@Ԭ@��y@��T@щ7@��@θR@��`@��m@�|�@�\)@�33@��H@�5?@�-@ɲ-@ɉ7@���@���@�j@�Q�@�;d@Ł@��@�K�@�M�@�@�I�@�S�@��@�{@�`B@��
@�;d@��y@�5?@�`B@��/@��P@�ff@��h@���@��`@��@�j@�b@���@�K�@�$�@��@��j@���@��@��@���@���@�l�@�-@��^@�x�@�7L@�j@��D@��+@�l�@�\)@���@���@�v�@���@�(�@�Ĝ@�7L@�?}@��j@�1@�\)@�K�@�ȴ@��@���@���@��+@�V@���@��@��T@���@��-@�X@���@���@���@���@���@���@�j@�  @��@��@�=q@�p�@�X@�?}@���@���@���@��;@�o@��@��j@���@�%@��@�|�@��@�K�@��@��\@�{@��#@�@���@�O�@��/@��@��m@��@���@��@�`B@�7L@��`@��D@��u@��9@��9@�j@��m@��@���@�|�@�dZ@�  @��@�1@�1'@�dZ@���@��@��-@�@��#@�v�@�ff@�$�@�hs@���@��@�%@��/@��@�z�@��m@�dZ@�+@���@�V@�M�@�V@�v�@���@���@���@��T@�7L@���@��D@�Q�@���@��F@�o@�v�@�n�@�{@�hs@��`@��9@��u@�Z@��
@��F@�C�@��#@�x�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�z�A�x�A�r�A�t�A�x�A�t�ÁA̅A̅Ȧ+Ȧ+Ȧ+Ȁ\A̓uA̓uA̅ÃÃA̅ÃA̅A̅Ả7A���A�oA�^5A˅A�-A��A�ȴAŃA�A�&�A��A��A��/AhA��A��^A�(�A�VA���A�K�A�(�A�`BA�A�oA��uA���A��jA���A�ffA��wA��uA�C�A�{A���A�dZA�{A��^A��A�VA��-A�33A��mA�ffA�&�A��mA��hA���A�XA���A�C�A�z�A��
A�"�A�?}A���A�7LA�S�A�oA�ȴA�+A��A��+A���A�ffA�Q�A��HA���A��A�C�A���A��A�jA��+A��yA��A�E�A���A��A��A��!A��RA��`A���A���A�ffA�I�A�ĜA�1'A�M�A��+A���A��A�x�A��yA��A��FA�$�A��/A���A�bA��A�1A}��A|-Azn�AxZAu�At1'Ar-ApbAn��AnbNAm`BAlVAk�hAkx�Aj�DAg�Af�AfbAe�Acl�Ab^5Aa�
Aa�^Aa?}A`E�A_�mA^ĜA\��AZ��AX��AW\)AV(�AUl�AT�AT��AT9XAS��AR�AQ�#AQK�AP�AO��AL��AKhsAJI�AHQ�AF�AE��AE33AC�mA@A�A>jA=|�A=K�A<�yA;�FA;?}A:��A:��A:��A9�-A8E�A7�hA7�A6-A5|�A4(�A3+A2�\A1�-A0��A0ffA/�hA.ĜA.jA-��A,��A+`BA*  A)�FA)�PA(�RA'�mA&��A%C�A$��A$1A"��A"(�A!�hA!VA �/A �\A A��A7LAQ�A\)A�9A�-A�uA�mAdZAbAC�AĜAbNA�`A=qA��A�jA�AZA��A�Ap�AA9XA��AA
ĜA
$�A�HA�A�A-A�HAA�AƨA?}A��A^5A �/@�ƨ@���@�{@���@���@�l�@�{@��u@�I�@��P@��@�A�@��@��#@�9@�@��@�{@�"�@�j@�=q@�=q@��@ۥ�@�=q@ّh@ش9@��
@�ƨ@�
=@�{@�/@Ԭ@��y@��T@щ7@��@θR@��`@��m@�|�@�\)@�33@��H@�5?@�-@ɲ-@ɉ7@���@���@�j@�Q�@�;d@Ł@��@�K�@�M�@�@�I�@�S�@��@�{@�`B@��
@�;d@��y@�5?@�`B@��/@��P@�ff@��h@���@��`@��@�j@�b@���@�K�@�$�@��@��j@���@��@��@���@���@�l�@�-@��^@�x�@�7L@�j@��D@��+@�l�@�\)@���@���@�v�@���@�(�@�Ĝ@�7L@�?}@��j@�1@�\)@�K�@�ȴ@��@���@���@��+@�V@���@��@��T@���@��-@�X@���@���@���@���@���@���@�j@�  @��@��@�=q@�p�@�X@�?}@���@���@���@��;@�o@��@��j@���@�%@��@�|�@��@�K�@��@��\@�{@��#@�@���@�O�@��/@��@��m@��@���@��@�`B@�7L@��`@��D@��u@��9@��9@�j@��m@��@���@�|�@�dZ@�  @��@�1@�1'@�dZ@���@��@��-@�@��#@�v�@�ff@�$�@�hs@���@��@�%@��/@��@�z�@��m@�dZ@�+@���@�V@�M�@�V@�v�@���@���@���@��T@�7L@���@��D@�Q�@���@��F@�o@�v�@�n�@�{@�hs@��`@��9@��u@�Z@��
@��F@�C�@��#@�x�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB[#B[#B[#B[#B\)B\)B\)B[#B[#B[#B[#B[#B]/B_;B`BB\)B[#B[#B[#B[#B[#B[#B_;B��BBS�BW
BM�B>wB'�B	7B��B.B,B!�B�B�BDBbB��B��B�dB�^B�qB�}B��BǮBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B�B�/B�BB�;B�BB�HB�NB�NB�;B�BB�
B��BÖB�9B��B�VB�DB~�Bq�Bm�BaHBbNBcTB_;BT�BO�BP�BL�B1'B"�B�B�BPBB��B�B��BĜB�jB�?B�B��B�DBs�BS�B>wB'�BDBB
��B
�B
�
B
��B
��B
��B
ĜB
�FB
�B
�VB
}�B
y�B
l�B
hsB
ffB
VB
L�B
@�B
6FB
+B
!�B
�B

=B
B
B	��B	��B	�B	�B	�fB	�B	��B	��B	��B	B	�qB	�^B	�XB	�FB	�'B	�B	��B	��B	��B	�bB	�PB	�B	�B	�B	~�B	|�B	z�B	u�B	q�B	o�B	l�B	e`B	W
B	O�B	H�B	@�B	9XB	49B	.B	#�B	�B	bB	JB	DB	1B	B	B	  B��B��B��B�B�B�B�sB�ZB�5B�#B�B��B��B��B��BȴBƨBÖB�wB�^B�FB�?B�3B�!B�B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�\B�PB�=B�1B�B�B�B}�B{�Bz�Bz�Bu�Bs�Bq�Bn�Bk�BgmBdZBcTBbNB`BB^5B]/B]/B]/B[#BYBVBQ�BM�BL�BL�BK�BJ�BI�BH�BF�BF�BF�BE�BD�BC�BB�BB�BC�BC�BC�BD�BE�BE�BD�BB�B@�B?}B?}B>wB<jB8RB:^B9XB:^B;dB;dB;dB=qB;dB;dB<jB<jB<jB=qB?}B>wB=qBA�BE�BG�BH�BI�BI�BJ�BO�BXB[#B]/B]/B`BBcTBdZBdZBe`Bk�Bq�Bt�Bt�Bt�Bt�Bt�Bw�Bx�B|�B}�B}�B}�B�B�B�B�B~�B�B�=B�\B�oB�uB�{B�{B��B��B�{B�{B�{B�{B�uB�{B�{B�{B��B��B��B��B�B�B�LB�LB�?B�XB�wBɺB��B��B�#B�BB�TB�ZB�`B�mB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	+B		7B	DB	DB	
=B		7B	%B	+B	oB	�B	!�B	8RB	<jB	B�B	S�B	]/B	bNB	cTB	cTB	dZB	dZB	e`B	e`B	gmB	gmB	ffB	dZB	bNB	`BB	`BB	`BB	_;B	_;B	aHB	bNB	cTB	cTB	cTB	ffB	gmB	jB	m�B	r�B	s�B	t�B	w�B	v�B	t�B	t�B	w�B	y�B	{�B	� B	�B	�B	�B	�B	�B	�B	�%B	�+B	�%B	�B	�B	�%B	�7B	�DB	�JB	�PB	�\B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B[#B[#B[#B[#B\)B\)B\)B[#B[#B[#B[#B[#B]/B_;B`BB\)B[#B[#B[#B[#B[#B[#B^5B��B+BXB]/BYBE�B/BPB��B5?B1'B"�B�B�BJB�BB��B�qB�wB��BBÖBɺB��B��B�B��B��B��B��B��B��B��B��B��B��B�B�/B�BB�NB�NB�HB�NB�TB�ZB�HB�TB�B��BƨB�LB��B�bB�VB�Bw�Bs�BdZBdZBe`BbNBXBP�BS�BXB6FB%�B�B�BhB1B��B��B��BǮB�wB�RB�FB��B��B�B[#BF�B49BVBB
��B
�B
�B
��B
��B
��B
ɺB
�dB
�3B
�{B
~�B
}�B
o�B
k�B
m�B
ZB
Q�B
F�B
>wB
0!B
(�B
�B
VB
%B
B	��B	��B	�B	�B	�B	�5B	��B	��B	��B	ƨB	�}B	�dB	�dB	�XB	�3B	�-B	�B	��B	��B	�{B	�bB	�+B	�B	�B	� B	}�B	}�B	w�B	r�B	p�B	o�B	m�B	ZB	R�B	N�B	D�B	<jB	7LB	2-B	/B	�B	uB	PB	PB	JB	B	B	B	  B	  B��B�B�B�B�B�sB�HB�/B�)B�B��B��B��BɺBȴBǮBÖB�}B�LB�FB�FB�3B�!B�B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�\B�PB�DB�+B�B�B� B}�B{�B~�Bw�Bu�Bt�Br�Bo�Bl�BgmBe`BdZBcTB`BB_;B^5B_;B_;B\)BZBVBR�BN�BN�BM�BL�BK�BM�BI�BH�BG�BF�BF�BF�BE�BE�BD�BE�BG�BF�BG�BG�BF�BF�BC�BA�BC�BA�B@�B=qB<jB;dB<jB<jB<jB<jB=qB<jB=qB>wB=qB?}B?}B@�B?}BA�BD�BG�BH�BH�BI�BJ�BK�BO�BYB\)B]/B]/BaHBcTBffBgmBhsBm�Bs�Bu�Bt�Bt�Bu�Bv�By�B{�B}�B~�B}�B� B�B�1B�B�B� B�B�=B�bB�uB�{B��B��B��B��B��B��B��B��B�uB��B��B��B��B��B��B��B�!B�B�LB�RB�?B�XB�jBȴB��B��B�#B�HB�ZB�`B�`B�sB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	+B		7B	JB	PB	PB	JB	+B	%B	hB	�B	�B	9XB	>wB	?}B	Q�B	^5B	cTB	dZB	cTB	dZB	dZB	ffB	ffB	gmB	hsB	gmB	e`B	cTB	aHB	`BB	aHB	`BB	_;B	aHB	bNB	dZB	dZB	cTB	ffB	hsB	jB	l�B	r�B	s�B	t�B	y�B	x�B	u�B	t�B	w�B	y�B	z�B	� B	�B	�B	�B	�B	�B	�B	�+B	�+B	�+B	�B	�%B	�+B	�=B	�DB	�JB	�PB	�VB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<D��<T��<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447032012010314470320120103144703  AO  ARGQ                                                                        20111130141030  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141030  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144703  IP                  G�O�G�O�G�O�                