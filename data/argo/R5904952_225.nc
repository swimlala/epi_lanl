CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:56Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  A�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  J`   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Q$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  W�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Y�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ``   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  b   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qP   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  20181005190556  20181005190556  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�����1   @��I��6@0��-V�c�p��
=1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   B  B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&�C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��C�  C��3C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  D   D y�D ��D� DfD�fDfD� DfD� D  D� D  D� D  D�fD  D� D	  D	� D
  D
� D  D� D��D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  Dy�D  Dy�D  D� D  D� D  D� DfD� D  D� D  D� D  D�fD  D� D  D� D  D� D   D � D ��D!� D"  D"�fD#  D#� D#��D$� D%  D%� D&  D&y�D'  D'� D'��D(y�D(��D)� D*  D*� D+  D+� D,  D,�fD-fD-�fD.  D.y�D/  D/�fD0fD0�fD1fD1� D2  D2�fD3  D3�fD4fD4� D5  D5�fD6  D6� D6��D7y�D7��D8� D8��D9y�D:  D:� D;  D;� D<fD<�fD=  D=� D>  D>� D?  D?�fD@  D@� DA  DA� DB  DB� DB��DC� DDfDD� DE  DE� DE��DF� DG  DGy�DH  DH� DH��DU  DU� DU��DV� DW  DW� DX  DX� DY  DY� DZ  DZy�D[  D[� D[��D\� D]  D]� D^fD^� D_  D_�fD`fD`� DafDa� DbfDb� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dgy�Dg��Dhy�Dh��Di� Dj  Dj� DkfDk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp�fDq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dyo\D�,{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BG�B!G�B)G�B0�HB9G�BAG�BIG�BQG�BYG�BaG�BiG�BqG�ByG�B���B���B���B���B���B���B���B���B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�Ck�CQ�C Q�C"Q�C$Q�C&k�C(k�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CP8RCRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`Q�CbQ�Cdk�CfQ�ChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�CvQ�Cxk�CzQ�C|Q�C~Q�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�)C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�5�C�5�C�5�C�5�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�)C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�(�C�(�C�5�C�(�C�)C�(�C�5�C�5�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�D {D �DD�{D�D��D�D�{D�D�{D{D�{D{D�{D{D��D{D�{D	{D	�{D
{D
�{D{D�{DD�{D{D�{D{D�{D{D�{D{D�{D{D��D{D�{D{D�{D{D�D{D�D{D�{D{D�{D{D�{D�D�{D{D�{D{D�{D{D��D{D�{D{D�{D{D�{D {D �{D!D!�{D"{D"��D#{D#�{D$D$�{D%{D%�{D&{D&�D'{D'�{D(D(�D)D)�{D*{D*�{D+{D+�{D,{D,��D-�D-��D.{D.�D/{D/��D0�D0��D1�D1�{D2{D2��D3{D3��D4�D4�{D5{D5��D6{D6�{D7D7�D8D8�{D9D9�D:{D:�{D;{D;�{D<�D<��D={D=�{D>{D>�{D?{D?��D@{D@�{DA{DA�{DB{DB�{DCDC�{DD�DD�{DE{DE�{DFDF�{DG{DG�DH{DH�{DIDU{DU�{DVDV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�D[{D[�{D\D\�{D]{D]�{D^�D^�{D_{D_��D`�D`�{Da�Da�{Db�Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�DhDh�DiDi�{Dj{Dj�{Dk�Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do�Do�{Dp{Dp��Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{Dw{Dw�{Dy��D�6�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AʃA�dZA�I�A�$�A�n�A�t�A��AǗ�A��A�&�A�5?A�33A�1AƸRAƏ\A�t�A�ZA�I�A�C�A�+A�A�`BA�bA�n�A��PA�O�A��TA�ƨA�;dA���A�5?A��A��A�M�A���A�jA�A��A�  A�Q�A���A���A��FA��/A��A�K�A��RA�33A��-A��^A���A��A�K�A��jA� �A��A��A��
A��;A��A�G�A�ffA�v�A|ffAw��Au�At  Ar�uAp~�Ak"�AiVAf�Ac��A`�A^ȴAZ��AX$�AUƨAP�!APA�AP��AO+AK|�AIp�AE�mADffABQ�A@9XA>z�A<^5A:�uA9t�A8��A7oA5|�A3`BA1%A/��A.�A.E�A-�#A-�PA,�A+�A)A'�TA%�A%C�A$�A$A�A#"�A"I�A ��A�A33A�A-AXA�/AO�A��A^5A��A�yA�!A�^A7LA%A�A�mA�#A��Al�A/A�AQ�A��A�RAVAv�AM�A��A33A
�/A
I�A	�;A	O�AffA;dA��A�DAE�AK�A��@��@�hs@��F@���@��D@���@�O�@� �@�+@�&�@�33@�~�@�5?@��@�Z@�dZ@�o@�~�@�v�@�v�@�ff@��@�hs@�V@��@��m@�E�@�@��@��@�X@�Z@ߥ�@���@���@�@݉7@���@�;d@�n�@�@�7L@؋D@�t�@�;d@��@��@�~�@�-@��@ղ-@Ձ@�/@��@�l�@��@�^5@�`B@υ@�^5@�`B@˝�@�@�V@��#@�`B@��@���@ȣ�@�I�@ǝ�@�
=@�ȴ@���@Ƈ+@�5?@��@őh@�G�@��@�b@öF@�dZ@�@\@��#@��7@�?}@��@�Z@�(�@�o@��^@�`B@��#@�V@�Q�@��F@�t�@�^5@�v�@�V@���@���@�V@�ff@�-@��7@��@�(�@���@���@���@��@���@�$�@���@���@�&�@�Z@��@�t�@�^5@�O�@�Z@���@���@�"�@���@�-@���@�p�@�Ĝ@��F@�t�@���@�ƨ@�C�@�+@��@���@�5?@�@��@�Z@��
@�t�@�+@�E�@�M�@�V@��+@�V@���@�%@��@�r�@�bN@�I�@�9X@�1'@���@��@��m@���@��@�33@��@��!@���@�n�@�M�@�-@�J@��#@�/@���@��j@��@���@�
=@���@��@�t�@�v�@�x�@��@���@���@�r�@�Q�@��D@���@��j@��D@��w@��F@�1@��m@��@�|�@�S�@�+@�o@���@�V@��9@�S�@�33@��H@���@�M�@���@�hs@�&�@�&�@��@��@��/@��j@��@��u@�9X@��
@���@�C�@��@�n�@�{@��h@�x�@�x�@�7L@��@���@�b@��
@���@�C�@���@�M�@�E�@��@�?}@�/@���@��`@��u@�z�@�j@�I�@��@��@�\)@�o@��R@�v�@�=q@�J@��h@��@�x�@�O�@��@���@��9@�bN@�1'@�  @���@��F@���@�|�@�;d@�o@�ȴ@���@�~(@~{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AʃA�dZA�I�A�$�A�n�A�t�A��AǗ�A��A�&�A�5?A�33A�1AƸRAƏ\A�t�A�ZA�I�A�C�A�+A�A�`BA�bA�n�A��PA�O�A��TA�ƨA�;dA���A�5?A��A��A�M�A���A�jA�A��A�  A�Q�A���A���A��FA��/A��A�K�A��RA�33A��-A��^A���A��A�K�A��jA� �A��A��A��
A��;A��A�G�A�ffA�v�A|ffAw��Au�At  Ar�uAp~�Ak"�AiVAf�Ac��A`�A^ȴAZ��AX$�AUƨAP�!APA�AP��AO+AK|�AIp�AE�mADffABQ�A@9XA>z�A<^5A:�uA9t�A8��A7oA5|�A3`BA1%A/��A.�A.E�A-�#A-�PA,�A+�A)A'�TA%�A%C�A$�A$A�A#"�A"I�A ��A�A33A�A-AXA�/AO�A��A^5A��A�yA�!A�^A7LA%A�A�mA�#A��Al�A/A�AQ�A��A�RAVAv�AM�A��A33A
�/A
I�A	�;A	O�AffA;dA��A�DAE�AK�A��@��@�hs@��F@���@��D@���@�O�@� �@�+@�&�@�33@�~�@�5?@��@�Z@�dZ@�o@�~�@�v�@�v�@�ff@��@�hs@�V@��@��m@�E�@�@��@��@�X@�Z@ߥ�@���@���@�@݉7@���@�;d@�n�@�@�7L@؋D@�t�@�;d@��@��@�~�@�-@��@ղ-@Ձ@�/@��@�l�@��@�^5@�`B@υ@�^5@�`B@˝�@�@�V@��#@�`B@��@���@ȣ�@�I�@ǝ�@�
=@�ȴ@���@Ƈ+@�5?@��@őh@�G�@��@�b@öF@�dZ@�@\@��#@��7@�?}@��@�Z@�(�@�o@��^@�`B@��#@�V@�Q�@��F@�t�@�^5@�v�@�V@���@���@�V@�ff@�-@��7@��@�(�@���@���@���@��@���@�$�@���@���@�&�@�Z@��@�t�@�^5@�O�@�Z@���@���@�"�@���@�-@���@�p�@�Ĝ@��F@�t�@���@�ƨ@�C�@�+@��@���@�5?@�@��@�Z@��
@�t�@�+@�E�@�M�@�V@��+@�V@���@�%@��@�r�@�bN@�I�@�9X@�1'@���@��@��m@���@��@�33@��@��!@���@�n�@�M�@�-@�J@��#@�/@���@��j@��@���@�
=@���@��@�t�@�v�@�x�@��@���@���@�r�@�Q�@��D@���@��j@��D@��w@��F@�1@��m@��@�|�@�S�@�+@�o@���@�V@��9@�S�@�33@��H@���@�M�@���@�hs@�&�@�&�@��@��@��/@��j@��@��u@�9X@��
@���@�C�@��@�n�@�{@��h@�x�@�x�@�7L@��@���@�b@��
@���@�C�@���@�M�@�E�@��@�?}@�/@���@��`@��u@�z�@�j@�I�@��@��@�\)@�o@��R@�v�@�=q@�J@��h@��@�x�@�O�@��@���@��9@�bN@�1'@�  @���@��F@���@�|�@�;d@�o@�ȴ@���@�~(@~{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	:^B	;dB	=qB	?}B	VB	l�B	}�B	��B	��B	�?B	ȴB	�B	�B	�BB	�sB	�B	��B
B
B
VB
o�BBVBVBZB_;BYBT�BO�B@�B1'B)�B!�B�BhB
=B��B�B�B�NB�BȴB�FB��B��B��B�Bl�BJ�B/B�BB
�sB
ƨB
�B
��B
�=B
t�B
O�B
@�B
%�B
	7B	�B	��B	��B	�oB	�=B	~�B	o�B	R�B	D�B	6FB	$�B	uB	B�B�TB�
BɺB�5B�B�`B�
BǮB�XB�'B�-B��B��B��B��B��B�{B�bB�PB�+B�B�B�B�B�B�B�B�1B�7B�uB��B��B��B��B�B�3B�LB�dB��B��BBǮBƨBƨBȴBȴB��BɺBǮBȴBȴBɺB��B��B��B�B�HB�fB�yB�yB�B�fB�B�B��B�B��B��B�B�B�B�B�/B�BB�BB�;B�B��B��B��B��BǮBŢB��B�qB�XB�LB�^B�wB��B��B��BĜBȴB��B��B��B��B��B�
B�B�B�B�B�)B�/B�;B�TB�mB�B�B�B�B�B�B��B��B��B��B	  B	B	1B		7B		7B		7B	
=B	DB	JB	PB	PB	PB	bB	oB	{B	�B	�B	�B	�B	�B	�B	!�B	$�B	'�B	+B	-B	/B	/B	2-B	49B	6FB	9XB	<jB	>wB	@�B	B�B	F�B	G�B	H�B	M�B	O�B	P�B	P�B	Q�B	XB	\)B	]/B	`BB	aHB	e`B	iyB	k�B	m�B	u�B	x�B	y�B	y�B	{�B	|�B	�B	�+B	�DB	��B	��B	�B	�B	�B	�'B	�B	�B	�'B	�FB	�XB	�jB	�^B	�jB	�wB	�}B	��B	��B	��B	��B	��B	�}B	ÖB	ŢB	ŢB	ĜB	ÖB	B	��B	��B	�}B	��B	ÖB	ƨB	ȴB	ɺB	ɺB	ȴB	ȴB	ȴB	ȴB	ǮB	ƨB	ƨB	ȴB	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�;B	�BB	�HB	�HB	�HB	�HB	�HB	�TB	�ZB	�fB	�`B	�ZB	�sB	�sB	�fB	�`B	�`B	�sB	�B	�sB	�fB	�fB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��AUG�B
1B
1B
+B
1B
1B
+B
+B
1B
	7B

=B

=B
DB
DB
JB
JB
JB
PB
PB
VB
\B
\B
bB
bB
hB
oB
oB
oB
uB
oB
oB
{B
uB
hB
hB
hB
hB
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
"�B
2G2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222   B	:^B	;dB	=qB	?}B	VB	l�B	}�B	��B	��B	�?B	ȴB	�B	�B	�BB	�sB	�B	��B
B
B
VB
o�BBVBVBZB_;BYBT�BO�B@�B1'B)�B!�B�BhB
=B��B�B�B�NB�BȴB�FB��B��B��B�Bl�BJ�B/B�BB
�sB
ƨB
�B
��B
�=B
t�B
O�B
@�B
%�B
	7B	�B	��B	��B	�oB	�=B	~�B	o�B	R�B	D�B	6FB	$�B	uB	B�B�TB�
BɺB�5B�B�`B�
BǮB�XB�'B�-B��B��B��B��B��B�{B�bB�PB�+B�B�B�B�B�B�B�B�1B�7B�uB��B��B��B��B�B�3B�LB�dB��B��BBǮBƨBƨBȴBȴB��BɺBǮBȴBȴBɺB��B��B��B�B�HB�fB�yB�yB�B�fB�B�B��B�B��B��B�B�B�B�B�/B�BB�BB�;B�B��B��B��B��BǮBŢB��B�qB�XB�LB�^B�wB��B��B��BĜBȴB��B��B��B��B��B�
B�B�B�B�B�)B�/B�;B�TB�mB�B�B�B�B�B�B��B��B��B��B	  B	B	1B		7B		7B		7B	
=B	DB	JB	PB	PB	PB	bB	oB	{B	�B	�B	�B	�B	�B	�B	!�B	$�B	'�B	+B	-B	/B	/B	2-B	49B	6FB	9XB	<jB	>wB	@�B	B�B	F�B	G�B	H�B	M�B	O�B	P�B	P�B	Q�B	XB	\)B	]/B	`BB	aHB	e`B	iyB	k�B	m�B	u�B	x�B	y�B	y�B	{�B	|�B	�B	�+B	�DB	��B	��B	�B	�B	�B	�'B	�B	�B	�'B	�FB	�XB	�jB	�^B	�jB	�wB	�}B	��B	��B	��B	��B	��B	�}B	ÖB	ŢB	ŢB	ĜB	ÖB	B	��B	��B	�}B	��B	ÖB	ƨB	ȴB	ɺB	ɺB	ȴB	ȴB	ȴB	ȴB	ǮB	ƨB	ƨB	ȴB	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�;B	�BB	�HB	�HB	�HB	�HB	�HB	�TB	�ZB	�fB	�`B	�ZB	�sB	�sB	�fB	�`B	�`B	�sB	�B	�sB	�fB	�fB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��AUG�B
1B
1B
+B
1B
1B
+B
+B
1B
	7B

=B

=B
DB
DB
JB
JB
JB
PB
PB
VB
\B
\B
bB
bB
hB
oB
oB
oB
uB
oB
oB
{B
uB
hB
hB
hB
hB
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
"�B
2G2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.32 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190556                              AO  ARCAADJP                                                                    20181005190556    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190556  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190556  QCF$                G�O�G�O�G�O�8000            