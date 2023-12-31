CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:24Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140824  20181024140824  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               nA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��%%��=1   @��%�@�,@4�hr��cԣ�
=q1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      nA   A   A   @�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BO��BX  B`ffBhffBo��Bw��B��B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C��3C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� D  D�fD  D� D  D� D  Dy�D��D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D   D � D!  D!� D"  D"�fD#  D#� D$  D$� D%  D%� D%��D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,y�D-  D-� D.  D.� D/  D/� D0  D0�fD1  D1� D2  D2� D3  D3�fD4fD4� D5  D5� D6  D6y�D7  D7� D8  D8� D9fD9�fD:  D:y�D:��D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDL  DL�fDMfDM�fDNfDN� DO  DO� DP  DP� DP��DQy�DQ��DRy�DS  DS� DT  DT� DU  DU� DVfDV� DV��DWy�DX  DX�fDY  DY� DZ  DZy�D[  D[� D[��D\� D]  D]�fD^  D^� D_  D_� D`  D`�fDa  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp�fDqfDq�fDrfDr�fDsfDs�fDt  Dt� Du  Du� Dv  Dv�fDw  Dw� Dw��Dy�D�H D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��A��A$��AD��Ad��A�z�A��A�z�A�z�A�z�A�z�A�z�A�z�B=qB	=qB=qB=qB!=qB)=qB1��B9=qBA=qBI=qBP�BY=qBa��Bi��Bp�Bx�B�k�B���B���B���B���B���B���B���B���B�k�B���B���B���B���B���B���B���BĞ�BȞ�B̞�BО�BԞ�B؞�Bܞ�B���B䞸B螸B잸B�B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\CO\CO\CO\CO\C5�CO\CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJ5�CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\CrO\CtO\CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C��C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C��C��C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�4{C�4{C�'�C�'�C��C��C�'�C�'�C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�D =D ��D�D��D�D�=D�D��D�D��D�D�qDqD��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D�=D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�qD�D��D�D��D �D ��D!�D!��D"�D"�=D#�D#��D$�D$��D%�D%��D&qD&��D'�D'�=D(�D(��D)�D)��D*�D*��D+�D+��D,�D,�qD-�D-��D.�D.��D/�D/��D0�D0�=D1�D1��D2�D2��D3�D3�=D4=D4��D5�D5��D6�D6�qD7�D7��D8�D8��D9=D9�=D:�D:�qD;qD;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK�=DL�DL�=DM=DM�=DN=DN��DO�DO��DP�DP��DQqDQ�qDRqDR�qDS�DS��DT�DT��DU�DU��DV=DV��DWqDW�qDX�DX�=DY�DY��DZ�DZ�qD[�D[��D\qD\��D]�D]�=D^�D^��D_�D_��D`�D`�=Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp=Dp�=Dq=Dq�=Dr=Dr�=Ds=Ds�=Dt�Dt��Du�Du��Dv�Dv�=Dw�Dw��Dx �Dy��D�Q�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�;dA�7LA�7LA�-A�VA��A��A�A�jA�JA���A��A��mA��/A���A���A���A�A�!A��A��A�9A�wA�ƨA���A��PA�z�A�|�A֣�A��TA���A��mAδ9A��A̝�A�bAə�AȾwA��/A��A�1'A���AĶFA���A��A�ƨA�A��A���A�bNA�I�A��9A��A� �A��\A��A�G�A�9XA��wA���A���A�r�A���A�1A�x�A�hsA���A�9XA�+A���A�dZA�S�A��A��9A�\)A���A��mA���A�ĜA���A�  A�E�A�A��mA�ffA�t�A�S�A��yA�A��uA�ĜA��hA��/A�l�A�1'A�  A�x�A�M�A�1A��A� �A�ƨA���A|�Ay+AxbAv��As�7An�Ak�AiXAh-Af�9Ad��AbQ�A`��A^A�A\�A\�RAY��AX(�AWVAT�ARv�AOdZALv�AH�/AD��AAA@^5A>�RA=��A=`BA=/A<��A;��A:=qA9��A9hsA9?}A8�9A7�A5��A4ffA2��A0{A.n�A,ZA+S�A*�DA)�
A)|�A(��A(�A'�
A'��A&r�A$jA#dZA"M�A!"�A�;A�uAO�A�+A��Al�A33A��Ar�A��A�^A��A��A"�A��A��A�FA�jA�AbA��AAdZA��AĜAƨA33AA�yA�^A�A�yAO�A\)A  A{A�9Ax�A	��A	"�A{A�A(�A��A\)A%AjAl�A J@���@�(�@��+@���@��7@���@���@��`@�o@�hs@�\)@�@�1'@�%@���@���@�V@�I�@�9X@�@�-@�-@�{@�J@���@��@�`B@�X@�x�@�X@���@�5?@��@�7L@�(�@��y@�r�@۝�@���@�n�@��@�hs@أ�@�Q�@�(�@�\)@Դ9@Ӯ@ҧ�@��@ѡ�@��@�1'@�33@ҏ\@�?}@�Q�@�p�@�Z@�1'@ʧ�@�%@�(�@�^5@���@��@�r�@� �@��@�^5@���@���@�1'@�K�@��#@��T@��@�1@�t�@�\)@��y@��H@���@�ȴ@�ȴ@��R@��R@���@��\@�~�@�^5@�E�@�$�@���@�@��@���@���@���@��@�9X@�Z@�A�@��
@��@���@��@�|�@�K�@�@��!@�v�@�5?@��@��#@��^@��@��/@�I�@��F@�
=@���@��h@�hs@�X@�G�@�/@�%@���@��j@��w@��@��@�v�@��-@��@��@�7L@���@��@�5?@�{@�@�J@�J@��#@��7@��@�A�@�l�@���@��+@��T@��-@��@�b@�Ĝ@�K�@�ȴ@���@���@��\@�M�@�{@���@��#@�x�@��;@�S�@�+@�33@���@���@���@�~�@�ff@�5?@��T@��h@�?}@��@��u@�(�@���@�ƨ@���@�|�@�S�@�K�@�K�@�S�@�S�@�t�@��@���@���@�C�@��!@�n�@�V@���@�p�@�p�@�p�@��@��h@��7@�?}@��@�  @�C�@��H@��!@�v�@�M�@��^@��@�G�@�/@�&�@���@�Ĝ@��@�j@�  @��
@��@�l�@�o@��@���@�E�@��@���@��7@�7L@�p�@��@��#@�X@�7L@�&�@��`@�b@���@�K�@��@��H@��y@��@��@��y@���@�~�@�{@��@��^@���@�x�@�G�@�&�@���@�z�@�I�@� �@�S�@�o@��@���@�E�@��^@�p�@�X@�V@��j@��u@�Z@��@���@��@���@�dZ@��H@���@�~�@��@u�@a��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�;dA�7LA�7LA�-A�VA��A��A�A�jA�JA���A��A��mA��/A���A���A���A�A�!A��A��A�9A�wA�ƨA���A��PA�z�A�|�A֣�A��TA���A��mAδ9A��A̝�A�bAə�AȾwA��/A��A�1'A���AĶFA���A��A�ƨA�A��A���A�bNA�I�A��9A��A� �A��\A��A�G�A�9XA��wA���A���A�r�A���A�1A�x�A�hsA���A�9XA�+A���A�dZA�S�A��A��9A�\)A���A��mA���A�ĜA���A�  A�E�A�A��mA�ffA�t�A�S�A��yA�A��uA�ĜA��hA��/A�l�A�1'A�  A�x�A�M�A�1A��A� �A�ƨA���A|�Ay+AxbAv��As�7An�Ak�AiXAh-Af�9Ad��AbQ�A`��A^A�A\�A\�RAY��AX(�AWVAT�ARv�AOdZALv�AH�/AD��AAA@^5A>�RA=��A=`BA=/A<��A;��A:=qA9��A9hsA9?}A8�9A7�A5��A4ffA2��A0{A.n�A,ZA+S�A*�DA)�
A)|�A(��A(�A'�
A'��A&r�A$jA#dZA"M�A!"�A�;A�uAO�A�+A��Al�A33A��Ar�A��A�^A��A��A"�A��A��A�FA�jA�AbA��AAdZA��AĜAƨA33AA�yA�^A�A�yAO�A\)A  A{A�9Ax�A	��A	"�A{A�A(�A��A\)A%AjAl�A J@���@�(�@��+@���@��7@���@���@��`@�o@�hs@�\)@�@�1'@�%@���@���@�V@�I�@�9X@�@�-@�-@�{@�J@���@��@�`B@�X@�x�@�X@���@�5?@��@�7L@�(�@��y@�r�@۝�@���@�n�@��@�hs@أ�@�Q�@�(�@�\)@Դ9@Ӯ@ҧ�@��@ѡ�@��@�1'@�33@ҏ\@�?}@�Q�@�p�@�Z@�1'@ʧ�@�%@�(�@�^5@���@��@�r�@� �@��@�^5@���@���@�1'@�K�@��#@��T@��@�1@�t�@�\)@��y@��H@���@�ȴ@�ȴ@��R@��R@���@��\@�~�@�^5@�E�@�$�@���@�@��@���@���@���@��@�9X@�Z@�A�@��
@��@���@��@�|�@�K�@�@��!@�v�@�5?@��@��#@��^@��@��/@�I�@��F@�
=@���@��h@�hs@�X@�G�@�/@�%@���@��j@��w@��@��@�v�@��-@��@��@�7L@���@��@�5?@�{@�@�J@�J@��#@��7@��@�A�@�l�@���@��+@��T@��-@��@�b@�Ĝ@�K�@�ȴ@���@���@��\@�M�@�{@���@��#@�x�@��;@�S�@�+@�33@���@���@���@�~�@�ff@�5?@��T@��h@�?}@��@��u@�(�@���@�ƨ@���@�|�@�S�@�K�@�K�@�S�@�S�@�t�@��@���@���@�C�@��!@�n�@�V@���@�p�@�p�@�p�@��@��h@��7@�?}@��@�  @�C�@��H@��!@�v�@�M�@��^@��@�G�@�/@�&�@���@�Ĝ@��@�j@�  @��
@��@�l�@�o@��@���@�E�@��@���@��7@�7L@�p�@��@��#@�X@�7L@�&�@��`@�b@���@�K�@��@��H@��y@��@��@��y@���@�~�@�{@��@��^@���@�x�@�G�@�&�@���@�z�@�I�@� �@�S�@�o@��@���@�E�@��^@�p�@�X@�V@��j@��u@�Z@��@���@��@���@�dZ@��H@���@�~�@��@u�@a��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
ɺB
ɺB
ɺB
ɺB
ȴB
ȴB
ȴB
ȴB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
ȴB
ȴB
��B
�
B
�yBBPBhsB�'B�3B��BɺB�^B�?B��B��B�5B�mB�B��B��BB1B	7BVBuB$�B2-B9XB[#BiyBgmBgmBe`BR�BH�B@�B33B(�B)�B0!B<jBL�BM�BI�B?}B7LB)�B!�B�BoBoB�B'�B(�B-B&�B#�B"�B)�B.B1'B/B%�B�BhB��B�#B��BÖB�3B��B�JBw�BN�B7LB2-B-B!�BJB
�sB
B
��B
�1B
k�B
:^B
�B
hB
B	�B	��B	�XB	�B	��B	��B	�DB	y�B	m�B	_;B	XB	YB	O�B	F�B	>wB	1'B	#�B	oB	B�B�)B�B�
B�
B��B��B��B��B��B��B��B��BɺBŢB�}B�dB�LB�!B��B��B��B��B��B��B��B��B��B�{B�oB�\B�\B�\B�PB�VB�PB�\B�\B�PB�VB�\B�bB�bB�bB�hB��B��B�B�9B��B�^B��B�-B��B��B��B�9B�jBȴBǮB��B�jB�LB�3B�'B�B�9B��BŢB�wBÖB��B��B�jB�LB�B��B��B��B��B��B��B��B�uB�hB�PB�=B�1B�1B�1B�1B�+B�B�B�B�=B��B��B�B��B�B�B�'B��B��B��B�B�!B�B�FB�jB�jB�wB�qB�}B��BBB�}B�B��B��B��B��B�B�B�B�!B�B�!B�LB�XB�jB�wBɺB�
B�HB�5B�/B�/B�ZB�TB�HB�BB�5B�sB�B�B�B�B��B��B��B��B��B��B	B	1B	�B	�B	'�B	)�B	.B	/B	0!B	/B	/B	/B	/B	/B	/B	0!B	1'B	33B	6FB	8RB	:^B	;dB	=qB	B�B	C�B	C�B	D�B	H�B	L�B	M�B	M�B	N�B	N�B	O�B	P�B	Q�B	R�B	T�B	W
B	YB	\)B	]/B	_;B	`BB	aHB	e`B	hsB	k�B	o�B	x�B	y�B	{�B	{�B	|�B	}�B	}�B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�JB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�-B	�9B	�FB	�dB	�dB	�dB	�qB	��B	��B	��B	��B	��B	��B	ĜB	ĜB	ĜB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�/B	�BB	�HB	�TB	�ZB	�ZB	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B
	7B
	7B
	7B
	7B
DB
DB
PB
B
pB
3�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
ɺB
ɺB
ɺB
ɺB
ȴB
ȴB
ȴB
ȴB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
ȴB
ȴB
��B
�
B
�yBBPBhsB�'B�3B��BɺB�^B�?B��B��B�5B�mB�B��B��BB1B	7BVBuB$�B2-B9XB[#BiyBgmBgmBe`BR�BH�B@�B33B(�B)�B0!B<jBL�BM�BI�B?}B7LB)�B!�B�BoBoB�B'�B(�B-B&�B#�B"�B)�B.B1'B/B%�B�BhB��B�#B��BÖB�3B��B�JBw�BN�B7LB2-B-B!�BJB
�sB
B
��B
�1B
k�B
:^B
�B
hB
B	�B	��B	�XB	�B	��B	��B	�DB	y�B	m�B	_;B	XB	YB	O�B	F�B	>wB	1'B	#�B	oB	B�B�)B�B�
B�
B��B��B��B��B��B��B��B��BɺBŢB�}B�dB�LB�!B��B��B��B��B��B��B��B��B��B�{B�oB�\B�\B�\B�PB�VB�PB�\B�\B�PB�VB�\B�bB�bB�bB�hB��B��B�B�9B��B�^B��B�-B��B��B��B�9B�jBȴBǮB��B�jB�LB�3B�'B�B�9B��BŢB�wBÖB��B��B�jB�LB�B��B��B��B��B��B��B��B�uB�hB�PB�=B�1B�1B�1B�1B�+B�B�B�B�=B��B��B�B��B�B�B�'B��B��B��B�B�!B�B�FB�jB�jB�wB�qB�}B��BBB�}B�B��B��B��B��B�B�B�B�!B�B�!B�LB�XB�jB�wBɺB�
B�HB�5B�/B�/B�ZB�TB�HB�BB�5B�sB�B�B�B�B��B��B��B��B��B��B	B	1B	�B	�B	'�B	)�B	.B	/B	0!B	/B	/B	/B	/B	/B	/B	0!B	1'B	33B	6FB	8RB	:^B	;dB	=qB	B�B	C�B	C�B	D�B	H�B	L�B	M�B	M�B	N�B	N�B	O�B	P�B	Q�B	R�B	T�B	W
B	YB	\)B	]/B	_;B	`BB	aHB	e`B	hsB	k�B	o�B	x�B	y�B	{�B	{�B	|�B	}�B	}�B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�JB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�-B	�9B	�FB	�dB	�dB	�dB	�qB	��B	��B	��B	��B	��B	��B	ĜB	ĜB	ĜB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�/B	�BB	�HB	�TB	�ZB	�ZB	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B
	7B
	7B
	7B
	7B
DB
DB
PB
B
pB
3�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140824                              AO  ARCAADJP                                                                    20181024140824    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140824  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140824  QCF$                G�O�G�O�G�O�0               