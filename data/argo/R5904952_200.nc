CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:51Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  S`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  mP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190551  20181005190551  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��i��'21   @��j'Ғ@1�/��w�c���`A�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @���@���A   A   A@  Aa��A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B��B(ffB0ffB8ffB@  BH  BP  BX  B`  Bg��Bo��Bx  B�  B�  B�33B�33B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C.  C0  C2  C4  C6  C8  C:  C<  C=�fC?�fCB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C��C��C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  Dy�D��D	� D
fD
� D  D�fD  D� D  Dy�D  D� D  D�fD  D� D  D�fD  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� DfD� D  D�fDfD� D��D� D fD � D ��D!� D"  D"� D#  D#� D$fD$� D%  D%� D&  D&� D'  D'y�D(  D(� D)  D)�fD*fD*� D*��D+y�D,  D,�fD-fD-�fD.  D.� D.��D/y�D0  D0�fD0��D1y�D2  D2� D2��D3� D4  D4� D5fD5�fD6fD6� D7  D7� D8  D8y�D8��D9y�D:  D:y�D;  D;� D<  D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DBfDB� DC  DC� DD  DD�fDEfDE� DF  DF� DF��DGy�DH  DH� DH��DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ�fDR  DR� DSfDS�fDT  DT� DU  DU� DV  DV� DW  DW� DXfDX� DX��DY� DZfDZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Dp  Dp�fDq  Dq� Dr  Dr�fDs  Dsy�Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw��Dyp D�M111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�{@�{A��A$��AD��Af=qA�Q�A�Q�A�Q�A��A�Q�A�Q�A�Q�A�Q�B(�B	(�B(�B(�B B)�\B1�\B9�\BA(�BI(�BQ(�BY(�Ba(�BhBpBy(�B��{B��{B�ǮB�ǮB��{B�ǮB�ǮB��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BĔ{BȔ{B̔{BД{BԔ{Bؔ{Bܔ{B��{B�{B�{B�{B�{B��{B��{B��{C.J=C0J=C2J=C4J=C6J=C8J=C:J=C<J=C>0�C@0�CBJ=CD0�CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=CfJ=ChJ=CjJ=ClJ=CnJ=CpJ=CrJ=CtJ=CvJ=CxJ=CzJ=C|J=C~J=C�RC�%C�%C�%C�%C�%C�RC�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�RC�%C�1�C�1�C�%C�%C�1�C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�1�C�1�C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�1�C�1�C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�1�C�%C�%C�RC�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%D )D ��D�D��D�D��D)D�)D�D��D�D��D�D��D�D��D�D�)D	)D	��D
�D
��D�D��D�D��D�D�)D�D��D�D��D�D��D�D��D�D�)D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D)D��D �D ��D!)D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'�)D(�D(��D)�D)��D*�D*��D+)D+�)D,�D,��D-�D-��D.�D.��D/)D/�)D0�D0��D1)D1�)D2�D2��D3)D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8�)D9)D9�)D:�D:�)D;�D;��D<�D<�)D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG)DG�)DH�DH��DI)DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO�)DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY)DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do�)Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds�)Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw�\Dy��D�Vg111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�E�A�VA�ZA�VA�^5A�dZA�`BA�`BA�^5A�`BA�`BA�hsA�l�A�n�A�p�A�p�A�p�A�n�A�dZA�bNA�bNA�dZA�hsA�ZA�XA�O�A�M�A�(�A�1A�  A�A��/AƑhAƁA�|�A�n�A�t�AƁAƇ+AƁA�  AǏ\A��HA�+A�Q�A�%A��A��A��A��A���Aİ!Aģ�Aė�A�|�A�p�A�S�A�9XA�1'A�A���A�A��HA�(�A���A�ZA��TA�bNA���A�hsA�ƨA��!A��A��wA�ZA�;dA�JA�K�A�;dA��A���A�z�A�ZA��A�t�A���A�5?A�bNA�\)A��A��FA��A}
=Aw�mAs|�Ar�HAr��ArQ�Ar�Apn�Ao?}Am
=Aj��Ah��Afn�Ae�Ac��AaS�A]�TAZQ�ATȴASl�AQ`BANE�AJ�AHI�AG
=AFZADbAA��A@ZA?��A>��A=x�A;��A:1'A7�PA6A�A4^5A3+A2(�A17LA/��A-��A+��A)��A(ZA&�yA&=qA%�;A$��A#�A"JA!�hA!Ar�A��A9XAJA�AVA�RA+A�HA9XAȴA1'A��A5?A1A�;A�PA��AAM�A�hA�mA
=qA	A	
=A��An�A�A��A�Ax�AȴA��A�AA��A$�A��Al�AffA\)A �\@��;@��@���@�Z@��@��@��w@��^@�1'@�&�@@�@웦@���@��@��@���@�u@睲@�7L@�+@�R@��#@�X@�Ĝ@���@�t�@��@��@�Q�@�S�@ڏ\@�{@��#@�?}@���@�1'@��
@�o@�J@�p�@���@�ƨ@�33@�@���@���@Гu@�9X@Ϯ@Χ�@�n�@�-@ͺ^@��`@̼j@˝�@�@�^5@�M�@�E�@���@�x�@�?}@���@�7L@�Ĝ@Ǖ�@���@�?}@ļj@ě�@�z�@� �@��@ÍP@�-@�x�@�X@�`B@��@�5?@���@�hs@���@�Q�@���@�+@�33@���@�-@��@�@�p�@�/@�Ĝ@���@��D@� �@��m@��@���@��@�V@� �@�t�@�@�@��@��@��@��j@��u@�r�@�Z@���@���@�@�t�@��m@�bN@��@�r�@�Z@���@�S�@�ȴ@�n�@�J@�J@��@��T@���@���@�7L@��@��j@�A�@�  @��
@��@�-@�{@���@���@�`B@��@��@�r�@��P@��!@�n�@���@�+@��@�b@�\)@�K�@���@�r�@�bN@���@��@�5?@���@��@�9X@�C�@��@�-@��h@�O�@���@�Ĝ@��@��u@�A�@��
@�K�@���@�V@�@���@��@�Ĝ@�  @�C�@�ȴ@�V@�=q@�5?@�=q@�=q@�=q@�5?@�=q@�V@���@�/@�%@���@���@�bN@�b@�ƨ@��@���@���@��P@�|�@��@�M�@�x�@�X@�O�@��@��/@��u@�(�@��@�b@���@�\)@�+@��!@�^5@�@��h@�O�@�?}@�/@�V@���@���@�z�@�9X@���@��;@���@�C�@��@��^@�x�@�/@���@��D@�bN@�Z@�Q�@�  @���@�l�@�;d@���@�=q@�-@��@���@�O�@��@���@���@��u@��D@��@���@�33@�@���@�v�@��@��-@��7@�p�@�G�@��`@�r�@��
@��P@�;d@�;d@���@�n�@�V@�M�@�K^@z~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�E�A�VA�ZA�VA�^5A�dZA�`BA�`BA�^5A�`BA�`BA�hsA�l�A�n�A�p�A�p�A�p�A�n�A�dZA�bNA�bNA�dZA�hsA�ZA�XA�O�A�M�A�(�A�1A�  A�A��/AƑhAƁA�|�A�n�A�t�AƁAƇ+AƁA�  AǏ\A��HA�+A�Q�A�%A��A��A��A��A���Aİ!Aģ�Aė�A�|�A�p�A�S�A�9XA�1'A�A���A�A��HA�(�A���A�ZA��TA�bNA���A�hsA�ƨA��!A��A��wA�ZA�;dA�JA�K�A�;dA��A���A�z�A�ZA��A�t�A���A�5?A�bNA�\)A��A��FA��A}
=Aw�mAs|�Ar�HAr��ArQ�Ar�Apn�Ao?}Am
=Aj��Ah��Afn�Ae�Ac��AaS�A]�TAZQ�ATȴASl�AQ`BANE�AJ�AHI�AG
=AFZADbAA��A@ZA?��A>��A=x�A;��A:1'A7�PA6A�A4^5A3+A2(�A17LA/��A-��A+��A)��A(ZA&�yA&=qA%�;A$��A#�A"JA!�hA!Ar�A��A9XAJA�AVA�RA+A�HA9XAȴA1'A��A5?A1A�;A�PA��AAM�A�hA�mA
=qA	A	
=A��An�A�A��A�Ax�AȴA��A�AA��A$�A��Al�AffA\)A �\@��;@��@���@�Z@��@��@��w@��^@�1'@�&�@@�@웦@���@��@��@���@�u@睲@�7L@�+@�R@��#@�X@�Ĝ@���@�t�@��@��@�Q�@�S�@ڏ\@�{@��#@�?}@���@�1'@��
@�o@�J@�p�@���@�ƨ@�33@�@���@���@Гu@�9X@Ϯ@Χ�@�n�@�-@ͺ^@��`@̼j@˝�@�@�^5@�M�@�E�@���@�x�@�?}@���@�7L@�Ĝ@Ǖ�@���@�?}@ļj@ě�@�z�@� �@��@ÍP@�-@�x�@�X@�`B@��@�5?@���@�hs@���@�Q�@���@�+@�33@���@�-@��@�@�p�@�/@�Ĝ@���@��D@� �@��m@��@���@��@�V@� �@�t�@�@�@��@��@��@��j@��u@�r�@�Z@���@���@�@�t�@��m@�bN@��@�r�@�Z@���@�S�@�ȴ@�n�@�J@�J@��@��T@���@���@�7L@��@��j@�A�@�  @��
@��@�-@�{@���@���@�`B@��@��@�r�@��P@��!@�n�@���@�+@��@�b@�\)@�K�@���@�r�@�bN@���@��@�5?@���@��@�9X@�C�@��@�-@��h@�O�@���@�Ĝ@��@��u@�A�@��
@�K�@���@�V@�@���@��@�Ĝ@�  @�C�@�ȴ@�V@�=q@�5?@�=q@�=q@�=q@�5?@�=q@�V@���@�/@�%@���@���@�bN@�b@�ƨ@��@���@���@��P@�|�@��@�M�@�x�@�X@�O�@��@��/@��u@�(�@��@�b@���@�\)@�+@��!@�^5@�@��h@�O�@�?}@�/@�V@���@���@�z�@�9X@���@��;@���@�C�@��@��^@�x�@�/@���@��D@�bN@�Z@�Q�@�  @���@�l�@�;d@���@�=q@�-@��@���@�O�@��@���@���@��u@��D@��@���@�33@�@���@�v�@��@��-@��7@�p�@�G�@��`@�r�@��
@��P@�;d@�;d@���@�n�@�V@�M�@�K^@z~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
!�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
&�B
'�B
'�B
'�B
&�B
)�B
)�B
,B
,B
49B
:^B
<jB
;dB
B�B
T�B
YB
[#B
k�B
z�B
�+B
��B
�B
��BbBuB+B	7BbB�B'�B0!B>wBL�BQ�BT�BVB[#B]/B_;B`BBjB�+B��B<�wB�BB�B��B�dB��B��B��B��B�{B�uB�\B�JB�B�B{�Bt�BG�B-BDBBB
��B
�#B
�RB
��B
�=B
�B
I�B
1B	�B	B	�B	��B	��B	��B	��B	��B	��B	�bB	�B	t�B	k�B	[#B	N�B	D�B	7LB	$�B	VB�B�`B�BŢB�9B��B��B��B��B��B��B��B��B��B��B��B�hB�bB�\B�JB�DB��B��B��B��B��B��B��B��B��B��B�B�B�B��B�B��B��B�B�B��B��B��B��B��B�B��B�B�9B�dB�}B��B��B�}B�wB�jB�XB�FB�3B�-B�'B�'B�-B�9B�LBŢB��B��B��B��B�B�B�B�
B�#B�5B�;B�BB�HB�HB�BB�5B�5B�NB�HB�NB�HB�BB�BB�;B�BB�BB�HB�NB�NB�ZB�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	%B	%B		7B	
=B	PB	VB	bB	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	$�B	$�B	$�B	'�B	(�B	(�B	)�B	+B	-B	0!B	33B	9XB	<jB	=qB	@�B	@�B	A�B	A�B	B�B	B�B	D�B	F�B	G�B	J�B	Q�B	ZB	ZB	[#B	[#B	ZB	aHB	aHB	iyB	n�B	p�B	r�B	x�B	~�B	�+B	�=B	�PB	�\B	�uB	�oB	�hB	�\B	�bB	�\B	�PB	�PB	�VB	�VB	�PB	�VB	�VB	�\B	�VB	�VB	�PB	�PB	�VB	�bB	�{B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�9B	�9B	�3B	�-B	�-B	�B	�'B	�-B	�3B	�3B	�3B	�3B	�-B	�-B	�'B	�B	�'B	�9B	�LB	�dB	�^B	��B	��B	B	ĜB	ȴB	��B	��B	��B	�
B	�
B	�
B	�
B	�B	�)B	�/B	�5B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�HB	�HB	�NB	�`B	�fB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�yB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
+B
1B
1B
1B
1B
	7B

=B

=B

=B

=B
DB
PB
VB
VB
VB
\B
bB
bB
bB
bB
bB
bB
bB
bB
bB
�B
#�222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
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
!�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
&�B
'�B
'�B
'�B
&�B
)�B
)�B
,B
,B
49B
:^B
<jB
;dB
B�B
T�B
YB
[#B
k�B
z�B
�+B
��B
�B
��BbBuB+B	7BbB�B'�B0!B>wBL�BQ�BT�BVB[#B]/B_;B`BBjB�+B��B<�wB�BB�B��B�dB��B��B��B��B�{B�uB�\B�JB�B�B{�Bt�BG�B-BDBBB
��B
�#B
�RB
��B
�=B
�B
I�B
1B	�B	B	�B	��B	��B	��B	��B	��B	��B	�bB	�B	t�B	k�B	[#B	N�B	D�B	7LB	$�B	VB�B�`B�BŢB�9B��B��B��B��B��B��B��B��B��B��B��B�hB�bB�\B�JB�DB��B��B��B��B��B��B��B��B��B��B�B�B�B��B�B��B��B�B�B��B��B��B��B��B�B��B�B�9B�dB�}B��B��B�}B�wB�jB�XB�FB�3B�-B�'B�'B�-B�9B�LBŢB��B��B��B��B�B�B�B�
B�#B�5B�;B�BB�HB�HB�BB�5B�5B�NB�HB�NB�HB�BB�BB�;B�BB�BB�HB�NB�NB�ZB�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	%B	%B		7B	
=B	PB	VB	bB	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	$�B	$�B	$�B	'�B	(�B	(�B	)�B	+B	-B	0!B	33B	9XB	<jB	=qB	@�B	@�B	A�B	A�B	B�B	B�B	D�B	F�B	G�B	J�B	Q�B	ZB	ZB	[#B	[#B	ZB	aHB	aHB	iyB	n�B	p�B	r�B	x�B	~�B	�+B	�=B	�PB	�\B	�uB	�oB	�hB	�\B	�bB	�\B	�PB	�PB	�VB	�VB	�PB	�VB	�VB	�\B	�VB	�VB	�PB	�PB	�VB	�bB	�{B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�9B	�9B	�3B	�-B	�-B	�B	�'B	�-B	�3B	�3B	�3B	�3B	�-B	�-B	�'B	�B	�'B	�9B	�LB	�dB	�^B	��B	��B	B	ĜB	ȴB	��B	��B	��B	�
B	�
B	�
B	�
B	�B	�)B	�/B	�5B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�HB	�HB	�NB	�`B	�fB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�yB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
+B
1B
1B
1B
1B
	7B

=B

=B

=B

=B
DB
PB
VB
VB
VB
\B
bB
bB
bB
bB
bB
bB
bB
bB
bB
�B
#�222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190551                              AO  ARCAADJP                                                                    20181005190551    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190551  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190551  QCF$                G�O�G�O�G�O�8000            