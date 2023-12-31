CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:16Z UW 3.1 conversion   
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  eP   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xP   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               [A   AO  20111130141224  20190522121826  1727_5046_091                   2C  D   APEX                            2143                            040306                          846 @ԋK��0 1   @ԋLF)��@7�I�^�c�E����1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk�fDl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Dz�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk�fDl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Dz�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�z�A�r�A�bNA�ZA��A�  A���Aд9Aа!AС�AБhAЉ7AЍPAЍPAЇ+AЅA�~�A�v�A�p�A�jA�G�AϺ^A�S�A���AΙ�A�r�A�=qA�bA�7LA�G�A�VA�%A�ƨA�
=A��A��mA���A�VA�O�A���A��mA�\)A�n�A��PA�n�A���A��hA�9XA�5?A�|�A��wA��TA�bNA�z�A��A�E�A�ȴA���A��yA��wA�jA�;dA���A��A���A�hsA�ƨA�ȴA�XA��PA��RA���A��A��/A��A�M�A�{A�l�A���A�O�A�l�A�
=A�(�A�p�A���A��A�K�A���A���A�p�A���A���A��^A�;dA�^5A�oA��FA�x�A���A���A�p�A���A�x�A�jA��hA�|�A��TA�dZA�A���A�oA�ȴA��uA��A�hsA��yA�$�A�1'A�A}��Az�+Aw��Av�\Au"�Aq�FAo�;An��Al��Ak?}Ai&�Ag��Af��Af��Af-Ad�HAd  Ac\)A`�jA_�
A_�A^ĜA]��A\�\A[�TA[x�AZ��AY�^AY�AX��AW�hAV��AU�hAT�RATZAS�AR�yAR  AP�/AO��AN1AM�PAL�`ALr�AKl�AI�AH��AG/AE��AD=qAC`BABv�AA��AAp�A@��A?��A?��A?;dA>�`A>ZA=�A=K�A<{A:�\A8��A8bNA7��A6jA4Q�A2�RA2�!A2�A2�+A1�A1��A1�A05?A.ffA-�A,�jA,5?A+�A+hsA*�uA*bA)t�A)A(�A'p�A&ȴA%�TA$JA#oA"��A!�PA!+A �A�yA  AdZA�RA$�A�A�hA+A�mA�A��A��A�AĜA�
AoA1A
=A��A�uA�
A��A�FA
�A	x�A��AZA�7A�;AĜA1A�7A33A��A{Al�A ��A ��A v�@���@�+@�O�@��\@���@�p�@���@�1@��H@��@�/@��m@�E�@�u@@���@�7L@�(�@�{@�9@���@�
=@�=q@��@�+@��@�C�@�Z@��/@���@�`B@��`@��@�+@���@�Q�@Χ�@�hs@̴9@��m@�dZ@���@�ff@ɲ-@��@��y@�M�@ēu@�  @� �@�1'@��;@��@���@��@��@��/@���@���@�@��9@�Z@�(�@�b@�ƨ@��@�t�@�l�@�S�@�K�@�n�@���@���@���@��`@�;d@�^5@��7@�V@�(�@��@��@���@�-@��u@���@�C�@�M�@��^@�p�@�X@�?}@�/@�7L@�7L@���@���@���@�5?@�~�@��@�E�@��@�j@��
@�
=@�n�@�E�@��@���@��T@�@�G�@�Z@�S�@��y@���@�V@��@���@�%@���@�j@�  @��@��y@��+@�=q@��@��@���@�p�@�V@���@��@�r�@�ƨ@��@�C�@��!@�~�@�ff@�5?@��-@��@���@��-@���@�&�@�%@��@� �@��@�A�@��/@�?}@��u@�C�@�G�@��@��/@���@���@�Ĝ@��9@��@��D@� �@�dZ@��y@��H@�C�@�"�@�C�@��@�&�@�9X@��T@��h@�bN@��@�@�C�@���@�E�@��#@��+@���@���@��@���@��+@�M�@�E�@���@���@���@�p�@��`@��9@��u@��@��@�Q�@��;@��@��@�\)@�+@��@���@���@�v�@�@�&�@�G�@��@�`B@�%@���@��j@��j@���@�1'@�1@�  @��;@��@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�z�A�r�A�bNA�ZA��A�  A���Aд9Aа!AС�AБhAЉ7AЍPAЍPAЇ+AЅA�~�A�v�A�p�A�jA�G�AϺ^A�S�A���AΙ�A�r�A�=qA�bA�7LA�G�A�VA�%A�ƨA�
=A��A��mA���A�VA�O�A���A��mA�\)A�n�A��PA�n�A���A��hA�9XA�5?A�|�A��wA��TA�bNA�z�A��A�E�A�ȴA���A��yA��wA�jA�;dA���A��A���A�hsA�ƨA�ȴA�XA��PA��RA���A��A��/A��A�M�A�{A�l�A���A�O�A�l�A�
=A�(�A�p�A���A��A�K�A���A���A�p�A���A���A��^A�;dA�^5A�oA��FA�x�A���A���A�p�A���A�x�A�jA��hA�|�A��TA�dZA�A���A�oA�ȴA��uA��A�hsA��yA�$�A�1'A�A}��Az�+Aw��Av�\Au"�Aq�FAo�;An��Al��Ak?}Ai&�Ag��Af��Af��Af-Ad�HAd  Ac\)A`�jA_�
A_�A^ĜA]��A\�\A[�TA[x�AZ��AY�^AY�AX��AW�hAV��AU�hAT�RATZAS�AR�yAR  AP�/AO��AN1AM�PAL�`ALr�AKl�AI�AH��AG/AE��AD=qAC`BABv�AA��AAp�A@��A?��A?��A?;dA>�`A>ZA=�A=K�A<{A:�\A8��A8bNA7��A6jA4Q�A2�RA2�!A2�A2�+A1�A1��A1�A05?A.ffA-�A,�jA,5?A+�A+hsA*�uA*bA)t�A)A(�A'p�A&ȴA%�TA$JA#oA"��A!�PA!+A �A�yA  AdZA�RA$�A�A�hA+A�mA�A��A��A�AĜA�
AoA1A
=A��A�uA�
A��A�FA
�A	x�A��AZA�7A�;AĜA1A�7A33A��A{Al�A ��A ��A v�@���@�+@�O�@��\@���@�p�@���@�1@��H@��@�/@��m@�E�@�u@@���@�7L@�(�@�{@�9@���@�
=@�=q@��@�+@��@�C�@�Z@��/@���@�`B@��`@��@�+@���@�Q�@Χ�@�hs@̴9@��m@�dZ@���@�ff@ɲ-@��@��y@�M�@ēu@�  @� �@�1'@��;@��@���@��@��@��/@���@���@�@��9@�Z@�(�@�b@�ƨ@��@�t�@�l�@�S�@�K�@�n�@���@���@���@��`@�;d@�^5@��7@�V@�(�@��@��@���@�-@��u@���@�C�@�M�@��^@�p�@�X@�?}@�/@�7L@�7L@���@���@���@�5?@�~�@��@�E�@��@�j@��
@�
=@�n�@�E�@��@���@��T@�@�G�@�Z@�S�@��y@���@�V@��@���@�%@���@�j@�  @��@��y@��+@�=q@��@��@���@�p�@�V@���@��@�r�@�ƨ@��@�C�@��!@�~�@�ff@�5?@��-@��@���@��-@���@�&�@�%@��@� �@��@�A�@��/@�?}@��u@�C�@�G�@��@��/@���@���@�Ĝ@��9@��@��D@� �@�dZ@��y@��H@�C�@�"�@�C�@��@�&�@�9X@��T@��h@�bN@��@�@�C�@���@�E�@��#@��+@���@���@��@���@��+@�M�@�E�@���@���@���@�p�@��`@��9@��u@��@��@�Q�@��;@��@��@�\)@�+@��@���@���@�v�@�@�&�@�G�@��@�`B@�%@���@��j@��j@���@�1'@�1@�  @��;@��@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBBBBBB%B1BDBDBJBDBDBPBVBVBVB\B\BbBbBuBoBJB	7BBB��B��B�yB�sB��B��B��B�B�wB�BB�`B�BB(�BF�B@�B?}BD�BC�BC�BS�B_;BYBYB�B��B��B�bB��B��B��B��B�B�LB�dB�^B�FB�JBt�BhsBaHBXBffB�1B��B�DB~�Bw�Bt�Bs�Bv�Bp�BhsBW
BM�BE�B8RB#�BbB+B��B�B�ZB�;B��BǮB�?B��B�\B|�Bm�BZBK�B<jB0!B�B\B
��B
�ZB
��B
��B
ƨB
��B
�dB
�FB
�3B
�'B
��B
��B
��B
�DB
}�B
k�B
]/B
L�B
>wB
6FB
/B
�B
hB
	7B	��B	��B	�B	�mB	�TB	�BB	�/B	�B	��B	��B	��B	�qB	�dB	�LB	�-B	�B	�B	��B	��B	��B	��B	��B	�hB	�=B	�B	�B	~�B	{�B	v�B	p�B	jB	cTB	\)B	YB	W
B	R�B	M�B	D�B	@�B	9XB	2-B	-B	(�B	$�B	"�B	�B	�B	�B	�B	�B	�B	oB	bB	JB	B��B��B��B�B�yB�NB�;B�;B�;B�5B�)B�B�B��B��B��BȴBƨBŢBB��B�}B�qB�^B�LB�?B�-B�B��B��B��B��B��B��B��B��B�uB�bB�VB�DB�+B�B�B� B|�B{�By�Bw�Bu�Br�Bo�Bl�BjBiyBhsBe`BdZB`BB_;B]/B\)BYBVBT�BT�BR�BR�BP�BP�BO�BO�BN�BM�BL�BK�BH�BI�BI�BI�BH�BG�BG�BG�BF�BE�BD�BD�BC�BC�BC�BB�BA�BB�BA�B@�B@�B?}BG�BJ�BI�BF�BH�BK�BL�BL�BM�BM�BN�BN�BP�BR�BS�BT�BVBVBVBVBW
BW
BT�BQ�BT�BW
BXBW
BT�BO�BM�BM�BN�BO�BO�BN�BP�BQ�BQ�BR�BW
BZB\)BbNBiyBn�Bo�Bo�Bm�Bk�BiyBiyBk�Bn�Bn�Bq�Bs�Bz�B|�B}�B~�B�B�1B�PB�VB�bB�bB�hB�oB�oB�oB�uB��B��B��B��B�B�3B�9B�?B�RB�dB��BBŢBƨBƨBƨBǮBȴB��B��B��B��B��B��B��B�
B�B�#B�5B�NB�`B�mB�sB�sB�yB�B�B�B�B�B��B��B��B	  B	B	B	B	+B	\B	�B	�B	�B	 �B	#�B	$�B	"�B	#�B	)�B	0!B	5?B	33B	1'B	0!B	0!B	1'B	49B	7LB	8RB	9XB	:^B	;dB	<jB	?}B	H�B	M�B	S�B	VB	XB	cTB	hsB	dZB	`BB	cTB	bNB	iyB	w�B	x�B	w�B	u�B	v�B	{�B	}�B	� B	�B	�B	�+B	�=B	�JB	�bB	�hB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�3B	�9B	�?B	�FB	�FB	�FB	�FB	�FB	�FB	�RB	�XB	�XB	�^B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BBBBBB+B	7BDBDBJBDBDBPBVBVBVB\B\BbBhB�B{BPBDBBBB��B�B�B��B��B��B�FBB�`B�B�B+B-BH�BB�BA�BH�BE�BD�BT�BcTB\)B\)B�B��B��B��B��B��B��B�B�!B�XB�jB�qB�}B�oBz�Bk�Be`BZBe`B�JB��B�bB�By�Bu�Bt�By�Bs�Bo�B[#BO�BJ�BA�B-B{BDB  B��B�`B�TB�B��B�qB��B��B�Br�B^5BP�BA�B49B%�B{BB
�yB
�B
��B
ȴB
B
�wB
�LB
�9B
�9B
�B
��B
��B
�bB
�B
p�B
e`B
S�B
B�B
:^B
8RB
 �B
�B
\B
B	��B	�B	�yB	�ZB	�HB	�HB	�B	��B	��B	B	�wB	�qB	�^B	�9B	�'B	�B	��B	��B	��B	��B	��B	�{B	�PB	�+B	�B	� B	~�B	y�B	s�B	n�B	hsB	^5B	[#B	YB	VB	S�B	G�B	D�B	>wB	6FB	0!B	,B	%�B	$�B	!�B	�B	�B	�B	�B	�B	uB	oB	\B		7B	B��B��B�B�B�mB�;B�;B�BB�BB�/B�)B�B�B��B��B��BǮBǮBŢBÖB��B�}B�wB�^B�RB�FB�?B�B��B��B��B��B��B��B��B��B�uB�oB�bB�7B�+B�B�B~�B|�B{�By�Bw�Bu�Br�Bp�Bm�Bk�Bk�BhsBgmBcTBaHB_;B_;B^5BYBW
BW
BS�BS�BS�BR�BQ�BP�BO�BM�BN�BN�BM�BJ�BJ�BK�BI�BI�BI�BI�BH�BH�BG�BF�BF�BD�BE�BE�BC�BC�BC�BA�BB�BB�BI�BL�BL�BJ�BJ�BM�BM�BM�BN�BO�BP�BP�BR�BS�BT�BVBW
BW
BW
BYBYBXBXBQ�BT�BW
BYBYB[#BR�BN�BN�BP�BR�BQ�BP�BQ�BR�BQ�BS�BXBZB\)BbNBiyBp�Bp�Bq�Bq�Bo�Bl�Bk�Bm�Bo�Bn�Br�Bt�Bz�B}�B� B� B�%B�7B�VB�\B�bB�bB�hB�oB�oB�uB��B��B��B��B��B�B�?B�?B�FB�^B�jB��BBŢBƨBƨBǮBɺB��B��B��B��B��B��B��B��B�B�B�)B�;B�TB�fB�mB�sB�sB�B�B�B�B�B�B��B��B��B	  B	B	B	B	+B	\B	�B	�B	�B	 �B	#�B	&�B	#�B	"�B	(�B	0!B	6FB	5?B	49B	0!B	0!B	1'B	49B	7LB	8RB	9XB	:^B	<jB	=qB	@�B	H�B	M�B	S�B	VB	VB	cTB	jB	hsB	aHB	e`B	bNB	e`B	w�B	y�B	x�B	v�B	u�B	{�B	}�B	� B	�B	�B	�+B	�=B	�PB	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�9B	�9B	�?B	�FB	�FB	�LB	�FB	�FB	�FB	�RB	�XB	�XB	�^B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447052012010314470520120103144706  AO  ARGQ                                                                        20111130141224  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141224  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144706  IP                  G�O�G�O�G�O�                