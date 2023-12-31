CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:12Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140812  20181024140812  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               )A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׹�כ�W1   @׹�n�@3\�1&��c�&�x��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      )A   A   A   @�33@�  A   AffA@  A`  A���A���A�  A�33A�33A�  A���A�  B   B  B  B  B   B'��B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  C   C  C  C  C  C
  C  C  C  C�fC�fC  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CA�fCC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C��C��C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D	  D	y�D	��D
y�D  D� D  Dy�D��Dy�D  D� D��D� D��D� D  D� D  D� D��D� D��D� D  D� D  D� D  D� D  D� D  D� DfD� D  Dy�D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D%��D&� D'  D'� D(  D(� D)  D)� D*  D*�fD+  D+� D,  D,� D-  D-� D.  D.� D.��D/y�D0  D0� D1fD1�fD2fD2�fD3  D3y�D3��D4� D5fD5� D6  D6� D6��D7� D8  D8� D9  D9� D9��D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DBy�DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDU  DUy�DV  DV� DW  DW� DX  DX� DY  DY� DZfDZ�fD[  D[� D\  D\� D]  D]� D^  D^y�D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� DkfDk� Dl  Dl� DmfDm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq�fDrfDr� Ds  Ds�fDt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dyu�D�I�D�%�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@�G�A��A#
=AD��Ad��A��A��A�Q�A��A��A�Q�A��A�Q�B(�B	(�B(�B(�B!(�B(B1(�B9(�BA(�BI(�BQ�\BY(�Ba(�Bi(�Bq(�By(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BĔ{BȔ{B̔{BД{BԔ{Bؔ{Bܔ{B��{B�{B�{B�{B�aHB�aHB�aHB��{C J=CJ=CJ=CJ=CJ=C
J=CJ=CJ=CJ=C0�C0�CJ=CJ=CJ=CJ=CJ=C J=C"J=C$c�C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4J=C6J=C80�C:J=C<J=C>J=C@J=CB0�CD0�CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=Cfc�ChJ=CjJ=ClJ=CnJ=CpJ=CrJ=CtJ=Cvc�CxJ=CzJ=C|J=C~J=C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�1�C�1�C�%C�1�C�1�C�1�C�%C�%C�1�C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�1�C�%C�%C�%C�RC�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D)D��D	�D	�)D
)D
�)D�D��D�D�)D)D�)D�D��D)D��D)D��D�D��D�D��D)D��D)D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�)D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&)D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/)D/�)D0�D0��D1�D1��D2�D2��D3�D3�)D4)D4��D5�D5��D6�D6��D7)D7��D8�D8��D9�D9��D:)D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB�)DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU�)DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^�)D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di�)Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dy�RD�R�D�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A�{A��A��A�p�A�%A��;A��/A���A���A�ĜA���A���A��A�&�A���A�`BA���A�
=A�{A�XA��A���A��HA�bA�XA���A��wA�?}A��`A���A��A��PA��A���A�"�A�$�A�E�A��A���A���A�bNA���A��wA��A��wA��A�{A��A�A��PA�K�A�33A�oA��A��wA�n�A��A���A�+A��`A��-A�~�A��-A��hA�v�A���A�n�A���A�7LA�C�A�1A��^A�1'A�%A�33A��A�K�A��#A��
A~��A|ffAwt�AuhsAs33AqXAp9XAol�An5?Al�uAjȴAi�hAh�jAg7LAe��Ac��Ab5?A`��A^�AY��AVffAT�9ASoAQ�APz�AN�AM��AK��AIp�AHM�AG/AFr�ACp�A@~�A?�A>1A=&�A;�7A:�A:-A9�mA9��A9��A9�A9l�A9
=A8bNA5?}A3��A2�`A1A/O�A.��A.�\A.ZA-��A,ffA+�^A+"�A)��A(��A(A�A'�-A'7LA&��A%hsA#��A#XA#
=A"�+A �A�TA�A\)A��A1A�`A�9A?}AO�A%A�A��A�
A�A�AI�Ax�A�A��A��Al�A�9A�A�AO�A
z�A
(�A	�A	|�A~�A�#A�A�`AQ�AC�AQ�A�A�yA(�AO�A ��A ��A �@��@�-@�r�@���@�33@��@�@�@�  @��y@@�n�@�-@홚@�I�@�@�!@���@�?}@�@��@�@�?}@�Z@�S�@݁@���@�
=@���@�5?@ٺ^@��@�ƨ@�C�@�ff@�@�V@Ԭ@�t�@ҟ�@϶F@��T@��`@�z�@��;@��y@�M�@ə�@�9X@���@�G�@��m@���@öF@Õ�@�|�@�|�@�|�@�C�@��@§�@+@�n�@�ff@�M�@���@��@��j@�@���@�
=@��!@�=q@��T@�x�@��@��@�ƨ@�
=@��H@��y@��!@�5?@���@��-@���@���@���@��@�hs@���@��@�Q�@�b@�b@�b@�b@��m@��
@��@��@�t�@��@��R@�{@��@�G�@�7L@�7L@�&�@���@�r�@�1@�;d@�ȴ@���@�n�@�5?@��T@�G�@��@��D@��@��
@�"�@�@�ȴ@�n�@�J@���@���@�X@���@�I�@��@��
@�C�@���@�@�@���@��@���@�z�@�j@�Q�@�z�@�A�@��;@�|�@�
=@���@��@��@��@��R@�{@��^@��^@��^@��^@��^@��-@���@���@��-@���@���@���@�p�@��/@�j@�9X@���@�t�@�
=@�@��y@���@���@�~�@�^5@�M�@�J@���@�j@��m@�@�$�@��@��#@��#@��^@�G�@�V@�Ĝ@��@�9X@���@���@�M�@�{@��@�@��^@��h@�?}@��@���@�j@�A�@��@��@�K�@�^5@�{@�X@��@�(�@�  @���@�t�@�S�@�o@���@��H@�E�@�{@��^@��@��@�1@�+@�@�ȴ@��!@��!@���@�n�@�ff@�V@�5?@�-@��@��@�O�@�%@���@��@��9@�r�@�I�@��@�1@��;@��@�
=@��@���@�n�@�v�@�n�@�V@�J@��-@���@��7@�p�@�7L@�%@��/@���@��9@��9@��@��u@�b@���@�"�@���@�E�@�5?@�$�@�J@���@���@��7@�x�@�p�@�`B@�O�@�?}@��@��@��/@���@��9@��u@�-@lS�@a�n11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A�{A��A��A�p�A�%A��;A��/A���A���A�ĜA���A���A��A�&�A���A�`BA���A�
=A�{A�XA��A���A��HA�bA�XA���A��wA�?}A��`A���A��A��PA��A���A�"�A�$�A�E�A��A���A���A�bNA���A��wA��A��wA��A�{A��A�A��PA�K�A�33A�oA��A��wA�n�A��A���A�+A��`A��-A�~�A��-A��hA�v�A���A�n�A���A�7LA�C�A�1A��^A�1'A�%A�33A��A�K�A��#A��
A~��A|ffAwt�AuhsAs33AqXAp9XAol�An5?Al�uAjȴAi�hAh�jAg7LAe��Ac��Ab5?A`��A^�AY��AVffAT�9ASoAQ�APz�AN�AM��AK��AIp�AHM�AG/AFr�ACp�A@~�A?�A>1A=&�A;�7A:�A:-A9�mA9��A9��A9�A9l�A9
=A8bNA5?}A3��A2�`A1A/O�A.��A.�\A.ZA-��A,ffA+�^A+"�A)��A(��A(A�A'�-A'7LA&��A%hsA#��A#XA#
=A"�+A �A�TA�A\)A��A1A�`A�9A?}AO�A%A�A��A�
A�A�AI�Ax�A�A��A��Al�A�9A�A�AO�A
z�A
(�A	�A	|�A~�A�#A�A�`AQ�AC�AQ�A�A�yA(�AO�A ��A ��A �@��@�-@�r�@���@�33@��@�@�@�  @��y@@�n�@�-@홚@�I�@�@�!@���@�?}@�@��@�@�?}@�Z@�S�@݁@���@�
=@���@�5?@ٺ^@��@�ƨ@�C�@�ff@�@�V@Ԭ@�t�@ҟ�@϶F@��T@��`@�z�@��;@��y@�M�@ə�@�9X@���@�G�@��m@���@öF@Õ�@�|�@�|�@�|�@�C�@��@§�@+@�n�@�ff@�M�@���@��@��j@�@���@�
=@��!@�=q@��T@�x�@��@��@�ƨ@�
=@��H@��y@��!@�5?@���@��-@���@���@���@��@�hs@���@��@�Q�@�b@�b@�b@�b@��m@��
@��@��@�t�@��@��R@�{@��@�G�@�7L@�7L@�&�@���@�r�@�1@�;d@�ȴ@���@�n�@�5?@��T@�G�@��@��D@��@��
@�"�@�@�ȴ@�n�@�J@���@���@�X@���@�I�@��@��
@�C�@���@�@�@���@��@���@�z�@�j@�Q�@�z�@�A�@��;@�|�@�
=@���@��@��@��@��R@�{@��^@��^@��^@��^@��^@��-@���@���@��-@���@���@���@�p�@��/@�j@�9X@���@�t�@�
=@�@��y@���@���@�~�@�^5@�M�@�J@���@�j@��m@�@�$�@��@��#@��#@��^@�G�@�V@�Ĝ@��@�9X@���@���@�M�@�{@��@�@��^@��h@�?}@��@���@�j@�A�@��@��@�K�@�^5@�{@�X@��@�(�@�  @���@�t�@�S�@�o@���@��H@�E�@�{@��^@��@��@�1@�+@�@�ȴ@��!@��!@���@�n�@�ff@�V@�5?@�-@��@��@�O�@�%@���@��@��9@�r�@�I�@��@�1@��;@��@�
=@��@���@�n�@�v�@�n�@�V@�J@��-@���@��7@�p�@�7L@�%@��/@���@��9@��9@��@��u@�b@���@�"�@���@�E�@�5?@�$�@�J@���@���@��7@�x�@�p�@�`B@�O�@�?}@��@��@��/@���@��9@��u@�-@lS�@a�n11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BPBPBJBPBPB
=B1BB��B�BB�BB�BS�BaHBe`Bk�BjBjBm�Bo�Bo�Bm�BiyBbNB`BB_;B\)BZBS�BK�BG�BG�BF�BC�B>wB8RB1'B%�B�B�BJBB��B�sB�5B�
B��B��BǮB�jB�B��B��B��B��B�uB�VB}�BhsBW
BO�BK�BG�BC�B8RB�BPBB
��B
�B
�;B
��B
��B
ȴB
��B
�jB
�B
�bB
v�B
`BB
A�B
+B
�B	��B	�sB	�B	ɺB	��B	�^B	�-B	��B	��B	�uB	�PB	�B	y�B	o�B	e`B	\)B	N�B	:^B	(�B	"�B	�B	{B	\B	1B	B��B�B�B�B�sB�;B�B�B��B��B��B��B��B��B��B��B��B��BȴBŢB��B��B��B��BÖBĜBÖBÖBBÖBB��BBBBÖBĜBƨB��B�
B�B�B�
B��B��B��B��B��B��BŢB�qB�qBƨBɺBɺB��BƨBŢBǮBĜB�}B�RB�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB�oB�oB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�-B�-B�3B�FB�FB�^B�^B�dB�dB�dB�wB�wB�}B��B��B��BÖBĜB�wB�^B�dB��BǮB��B��B��B�B�#B�mB�B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	1B		7B	%B	B	DB	PB	bB	uB	�B	�B	�B	!�B	'�B	)�B	-B	.B	2-B	5?B	6FB	6FB	6FB	7LB	8RB	9XB	C�B	G�B	J�B	P�B	P�B	P�B	P�B	Q�B	R�B	R�B	R�B	R�B	S�B	W
B	]/B	aHB	cTB	dZB	dZB	dZB	e`B	gmB	hsB	jB	m�B	q�B	t�B	u�B	v�B	y�B	y�B	z�B	{�B	{�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�7B	�7B	�7B	�=B	�DB	�VB	�\B	�bB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�3B	�RB	�qB	�wB	�}B	B	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�;B	�HB	�NB	�ZB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B
DB
DB
DB
JB
JB
JB
PB
PB
VB
\B
bB
bB
bB
hB
hB
hB
hB
hB
hB
hB
hB
hB
oB
oB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
KB
-�B
7f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BPBPBJBPBPB
=B1BB��B�BB�BB�BS�BaHBe`Bk�BjBjBm�Bo�Bo�Bm�BiyBbNB`BB_;B\)BZBS�BK�BG�BG�BF�BC�B>wB8RB1'B%�B�B�BJBB��B�sB�5B�
B��B��BǮB�jB�B��B��B��B��B�uB�VB}�BhsBW
BO�BK�BG�BC�B8RB�BPBB
��B
�B
�;B
��B
��B
ȴB
��B
�jB
�B
�bB
v�B
`BB
A�B
+B
�B	��B	�sB	�B	ɺB	��B	�^B	�-B	��B	��B	�uB	�PB	�B	y�B	o�B	e`B	\)B	N�B	:^B	(�B	"�B	�B	{B	\B	1B	B��B�B�B�B�sB�;B�B�B��B��B��B��B��B��B��B��B��B��BȴBŢB��B��B��B��BÖBĜBÖBÖBBÖBB��BBBBÖBĜBƨB��B�
B�B�B�
B��B��B��B��B��B��BŢB�qB�qBƨBɺBɺB��BƨBŢBǮBĜB�}B�RB�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB�oB�oB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�-B�-B�3B�FB�FB�^B�^B�dB�dB�dB�wB�wB�}B��B��B��BÖBĜB�wB�^B�dB��BǮB��B��B��B�B�#B�mB�B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	1B		7B	%B	B	DB	PB	bB	uB	�B	�B	�B	!�B	'�B	)�B	-B	.B	2-B	5?B	6FB	6FB	6FB	7LB	8RB	9XB	C�B	G�B	J�B	P�B	P�B	P�B	P�B	Q�B	R�B	R�B	R�B	R�B	S�B	W
B	]/B	aHB	cTB	dZB	dZB	dZB	e`B	gmB	hsB	jB	m�B	q�B	t�B	u�B	v�B	y�B	y�B	z�B	{�B	{�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�7B	�7B	�7B	�=B	�DB	�VB	�\B	�bB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�3B	�RB	�qB	�wB	�}B	B	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�;B	�HB	�NB	�ZB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B
DB
DB
DB
JB
JB
JB
PB
PB
VB
\B
bB
bB
bB
hB
hB
hB
hB
hB
hB
hB
hB
hB
oB
oB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
KB
-�B
7f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140812                              AO  ARCAADJP                                                                    20181024140812    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140812  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140812  QCF$                G�O�G�O�G�O�0               