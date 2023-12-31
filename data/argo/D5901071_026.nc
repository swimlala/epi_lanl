CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:58Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135640  20190522121825  1727_5046_026                   2C  D   APEX                            2143                            040306                          846 @�7M6< 1   @�7N����@7f$�/��c�����1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B0ffB8ffB@��BH  BO��BW��B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D/��D0y�D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DAfDA�fDB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DT��DUy�DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B0ffB8ffB@��BH  BO��BW��B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D/��D0y�D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DAfDA�fDB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DT��DUy�DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AҾwAҾwA���A���Aҕ�A�A�A���A�ĜA���AѲ-Aѥ�AѓuAѓuAѕ�Aї�Aѕ�Aљ�AэPA�z�A�hsA�bNA�VA�5?A��A���A˺^A�Q�AǓuA���A�%Aę�A�\)A���A��uA���A�A�I�A���A��7A�VA��RA��A�  A�ffA��A�A��!A�jA��!A�ȴA�ȴA��A���A���A��/A���A��#A���A�`BA�bA��A�{A��wA�r�A���A��FA��7A�l�A�O�A�7LA���A�\)A��RA�E�A���A�G�A��A�XA�1A��^A��DA�ZA�"�A��-A�33A��!A�ZA��TA��RA��jA��+A�"�A��A��jA�"�A�oA���A�=qA��A��A�v�A�?}A��A���A�-A�S�A���A��A�{A��PA���A�\)A�ƨA��PA�|�A�A��TA���A��;A���A���A�O�A�dZA��A���A�XA�{A~bAz��Av��At��Ar�9Arr�ArbNAq��Ap^5Ao��An�`Alr�Ai�7Ai+AhAfI�Ac&�Aa��A`n�A_;dA\�/A\{A[p�AYl�AY/AY��AY��AZ �AY��AY�AY�AW�AU�7ATȴAS�
ARĜAR �AQ�PAP�AO�hAM`BAKx�AJ9XAHVAGoAF�AF��AE��AC?}AA��A@v�A@1A?�A>�uA=�PA<9XA;
=A:ffA9S�A7�TA2ZA1O�A0��A/�;A/dZA.��A.�jA.�uA.r�A.9XA,��A+�A*�A*�A)/A(I�A'��A&�A& �A$z�A#��A#�PA"�\A �/A jA�-A�+A��A��A��AffA�AƨA��A�DA�A�wAoAbA�PA`BAK�A&�Ar�A�\AhsA�A��A�RAdZA9XA�hA
�A
I�A	�A	�A	/A�AbNA��A`BA�HA  AG�AE�A33A n�@��@�C�@�M�@�@���@��;@�X@��@�K�@�n�@�V@�
=@�{@�F@�S�@��@�+@�h@�z�@��@�F@��@�33@��@ᙚ@�j@�1@�t�@�;d@��@�M�@ݡ�@��@ٲ-@�&�@�;d@���@�K�@���@�O�@Ь@��@Гu@�?}@ǶF@�=q@�=q@�$�@��T@�G�@�?}@�7L@ļj@��
@�33@�M�@��^@��@�1'@�dZ@��+@���@�&�@��j@��@���@�"�@���@�ff@�1'@��H@�33@�S�@��F@�b@���@��\@�Ĝ@�O�@�\)@��y@�v�@�@�J@�J@�@��@��#@�&�@�/@�?}@��/@��9@��@��9@��@�Ĝ@��@� �@�b@��@���@��;@�"�@���@�ff@�E�@��@�A�@�1@��F@�ȴ@�x�@���@�M�@�  @�-@��@�j@�(�@�K�@�;d@��@�@��;@�\)@�C�@��
@�j@�j@��u@�9X@�1'@�b@���@�S�@�o@���@��@�@�%@�j@�9X@� �@��@��@�dZ@�O�@���@��@�ƨ@��@��P@�dZ@��@��@�@�
=@�@��H@���@��+@��\@�~�@�ff@�V@�M�@�{@���@�G�@�V@���@� �@��w@��w@��@��H@��+@��@��^@��7@���@�(�@���@��w@��@��w@���@�|�@�K�@�33@��@��R@�v�@�n�@���@��+@�n�@�~�@��@��@���@��7@�p�@�p�@�p�@�x�@��@�x�@�?}@���@���@��@�Q�@�ƨ@��@�@��H@���@��+@�5?@��-@��@�7L@��/@�1'@�Q�@�bN@�A�@�1@��
@��P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AҾwAҾwA���A���Aҕ�A�A�A���A�ĜA���AѲ-Aѥ�AѓuAѓuAѕ�Aї�Aѕ�Aљ�AэPA�z�A�hsA�bNA�VA�5?A��A���A˺^A�Q�AǓuA���A�%Aę�A�\)A���A��uA���A�A�I�A���A��7A�VA��RA��A�  A�ffA��A�A��!A�jA��!A�ȴA�ȴA��A���A���A��/A���A��#A���A�`BA�bA��A�{A��wA�r�A���A��FA��7A�l�A�O�A�7LA���A�\)A��RA�E�A���A�G�A��A�XA�1A��^A��DA�ZA�"�A��-A�33A��!A�ZA��TA��RA��jA��+A�"�A��A��jA�"�A�oA���A�=qA��A��A�v�A�?}A��A���A�-A�S�A���A��A�{A��PA���A�\)A�ƨA��PA�|�A�A��TA���A��;A���A���A�O�A�dZA��A���A�XA�{A~bAz��Av��At��Ar�9Arr�ArbNAq��Ap^5Ao��An�`Alr�Ai�7Ai+AhAfI�Ac&�Aa��A`n�A_;dA\�/A\{A[p�AYl�AY/AY��AY��AZ �AY��AY�AY�AW�AU�7ATȴAS�
ARĜAR �AQ�PAP�AO�hAM`BAKx�AJ9XAHVAGoAF�AF��AE��AC?}AA��A@v�A@1A?�A>�uA=�PA<9XA;
=A:ffA9S�A7�TA2ZA1O�A0��A/�;A/dZA.��A.�jA.�uA.r�A.9XA,��A+�A*�A*�A)/A(I�A'��A&�A& �A$z�A#��A#�PA"�\A �/A jA�-A�+A��A��A��AffA�AƨA��A�DA�A�wAoAbA�PA`BAK�A&�Ar�A�\AhsA�A��A�RAdZA9XA�hA
�A
I�A	�A	�A	/A�AbNA��A`BA�HA  AG�AE�A33A n�@��@�C�@�M�@�@���@��;@�X@��@�K�@�n�@�V@�
=@�{@�F@�S�@��@�+@�h@�z�@��@�F@��@�33@��@ᙚ@�j@�1@�t�@�;d@��@�M�@ݡ�@��@ٲ-@�&�@�;d@���@�K�@���@�O�@Ь@��@Гu@�?}@ǶF@�=q@�=q@�$�@��T@�G�@�?}@�7L@ļj@��
@�33@�M�@��^@��@�1'@�dZ@��+@���@�&�@��j@��@���@�"�@���@�ff@�1'@��H@�33@�S�@��F@�b@���@��\@�Ĝ@�O�@�\)@��y@�v�@�@�J@�J@�@��@��#@�&�@�/@�?}@��/@��9@��@��9@��@�Ĝ@��@� �@�b@��@���@��;@�"�@���@�ff@�E�@��@�A�@�1@��F@�ȴ@�x�@���@�M�@�  @�-@��@�j@�(�@�K�@�;d@��@�@��;@�\)@�C�@��
@�j@�j@��u@�9X@�1'@�b@���@�S�@�o@���@��@�@�%@�j@�9X@� �@��@��@�dZ@�O�@���@��@�ƨ@��@��P@�dZ@��@��@�@�
=@�@��H@���@��+@��\@�~�@�ff@�V@�M�@�{@���@�G�@�V@���@� �@��w@��w@��@��H@��+@��@��^@��7@���@�(�@���@��w@��@��w@���@�|�@�K�@�33@��@��R@�v�@�n�@���@��+@�n�@�~�@��@��@���@��7@�p�@�p�@�p�@�x�@��@�x�@�?}@���@���@��@�Q�@�ƨ@��@�@��H@���@��+@�5?@��-@��@�7L@��/@�1'@�Q�@�bN@�A�@�1@��
@��P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBE�BE�BE�BD�BE�BH�BK�BL�BL�BL�BK�BJ�BK�BL�BM�BN�BO�BO�BN�BM�BL�BL�BL�BP�BJ�BF�BB�BH�BM�BVBVBYBffBs�B{�B|�B~�B�JB�bB�oB��B��B��B�B�'B�'B�-B�RBÖBĜBȴB��B��B�
B�HB�BB�`B�B��B��BB
=B\BoB�B�B�B�B�B�B�B�B!�B!�B"�B%�B#�B"�B �B"�B"�B#�B"�B,B%�B�B�BoBhBuB{BDB��B�NBƨB�LB��B��B}�BW
BK�BF�BB�B?}B49B�BJB��B�sB��B��B}�Bo�BH�B9XB0!B�B+B
�B
ɺB
�3B
�uB
�B
�B
x�B
dZB
O�B
<jB
(�B
{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	ǮB	�3B	��B	�\B	�+B	� B	t�B	s�B	n�B	_;B	bNB	x�B	� B	�uB	��B	��B	��B	��B	�bB	�7B	�B	}�B	z�B	v�B	q�B	k�B	_;B	N�B	D�B	9XB	33B	1'B	,B	!�B	uB	DB	1B	
=B	
=B	%B	B��B�B�B�ZB�BB�wB�dB�RB�?B�9B�-B�'B�!B�B��B��B��B��B��B��B�oB�oB�bB�VB�VB�JB�DB�DB�=B�1B�+B�%B�%B�%B�B�B�B�B�B�B� B� B� B�B�B� B~�B{�By�Bx�Bz�B|�B~�B~�B� B� B~�B|�B|�Bz�Bx�Bv�Bw�Bv�Bu�Bt�Br�Bo�Bo�Bn�Bn�Bl�Bm�Bo�Bp�Bn�Bl�Bo�BhsBe`Be`BcTB`BBffBz�B}�Bz�By�Bz�B{�B|�B}�B}�B}�B�B�+B�=B�DB�JB�PB�\B�bB�oB�\B�DB�+B�B�%B� Bs�Bz�B��B��B��B�bB�B�B�%B�7B�VB�uB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�\B�\B�VB�hB��B��B��B��B��B��B�B�-B�FB�LB�XB�dB�qB�}BŢB��B��B��B��B��B�B�B�B�B�#B�BB�HB�HB�5B�B�B�B�)B�/B�/B�)B�)B�/B�HB�NB�yB��B��B��B	  B	1B	\B	�B	�B	�B	�B	!�B	!�B	"�B	#�B	&�B	-B	2-B	33B	49B	5?B	6FB	;dB	<jB	9XB	8RB	:^B	>wB	A�B	C�B	C�B	E�B	H�B	L�B	N�B	O�B	Q�B	R�B	T�B	W
B	XB	ZB	[#B	[#B	^5B	aHB	bNB	bNB	e`B	gmB	iyB	k�B	m�B	n�B	o�B	p�B	r�B	t�B	y�B	|�B	}�B	� B	�B	�%B	�1B	�7B	�=B	�DB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�9B	�FB	�RB	�XB	�XB	�dB	�qB	�qB	�}B	��B	ÖB	ŢB	ǮB	ɺB	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BE�BE�BE�BE�BG�BJ�BK�BL�BL�BL�BK�BJ�BK�BL�BM�BN�BO�BO�BN�BM�BL�BL�BM�BQ�BO�BI�BK�BO�BQ�BXB\)BaHBm�Bw�B|�B� B�%B�\B�oB��B��B��B�B�'B�-B�'B�3B�dBǮBȴB��B��B��B�B�NB�ZB�fB�B��B  B+BJBhB�B�B�B�B�B�B�B�B!�B#�B#�B$�B'�B&�B#�B!�B#�B#�B$�B$�B/B(�B�B�BuBhB{B�BhB��B�B��B�jB��B��B�=BYBL�BG�BC�BD�B?}B�BbBB�B�
B�RB�B|�BN�B<jB5?B!�B\B
��B
��B
�wB
��B
�+B
�B
~�B
jB
VB
E�B
33B
�B
B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	��B	�jB	��B	�oB	�=B	�B	v�B	u�B	s�B	`BB	aHB	x�B	~�B	�{B	��B	��B	��B	��B	�oB	�DB	�%B	� B	|�B	x�B	u�B	q�B	dZB	R�B	I�B	<jB	49B	2-B	/B	'�B	�B	VB		7B	JB	PB		7B	B��B�B�B�yB�sBŢB��B�qB�^B�FB�?B�3B�-B�'B�'B�B��B��B��B��B��B��B�{B��B�bB�\B�bB�hB�PB�JB�DB�7B�1B�+B�+B�%B�%B�B�B�B�B�B�B�B�B�B�B�B�B|�Bz�B~�B� B�B�B�B�B�B}�B}�B|�B~�B{�Bz�Bw�Bw�Bw�Bu�Bs�Bs�Bq�Bs�Bq�Bo�Bp�Br�Bq�Bq�Bu�Bm�BgmBhsBgmBbNBcTB{�B�B}�B{�Bz�B|�B|�B}�B~�B� B�B�1B�DB�JB�JB�VB�bB�hB�{B�uB�JB�=B�+B�=B�1Bt�Bu�B��B��B��B��B�B�B�%B�=B�\B�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�bB�\B�hB��B��B��B��B��B��B�B�3B�FB�LB�XB�dB�qB�}BŢB��B��B��B��B��B�
B�
B�B�B�/B�HB�NB�TB�BB�)B�B�/B�;B�;B�5B�/B�5B�/B�NB�NB�sB��B��B��B��B	1B	\B	�B	�B	�B	 �B	"�B	!�B	"�B	$�B	'�B	.B	33B	33B	49B	5?B	6FB	<jB	?}B	:^B	9XB	;dB	>wB	A�B	C�B	D�B	E�B	H�B	L�B	N�B	O�B	R�B	R�B	T�B	W
B	XB	ZB	[#B	\)B	_;B	bNB	cTB	bNB	e`B	hsB	iyB	l�B	n�B	o�B	p�B	q�B	r�B	u�B	z�B	|�B	}�B	� B	�B	�%B	�1B	�7B	�=B	�DB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�3B	�9B	�FB	�RB	�XB	�^B	�jB	�qB	�wB	��B	B	ÖB	ŢB	ǮB	ɺB	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<49X<u<#�
<T��<#�
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
<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446432012010314464320120103144643  AO  ARGQ                                                                        20111130135640  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135640  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144643  IP                  G�O�G�O�G�O�                