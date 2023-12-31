CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:16Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140816  20181024140816  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               BA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��$����1   @��%@yoL@2T��E��c���R1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      BA   A   A   @���@�33A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CB�CD  CF  CH  CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl�Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	fD	� D
  D
� D  D� D  D� D  D� DfD�fDfD�fD  Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'�fD(  D(y�D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D2��D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;fD;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DCfDC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK�fDL  DL� DM  DM� DN  DN� DN��DOy�DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DVy�DW  DW� DX  DX�fDYfDY�fDZ  DZy�DZ��D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da�fDb  Db� DcfDc� Dd  Dd� De  De� Df  Df� Dg  Dg� DhfDh� Di  Di�fDjfDj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� DsfDs�fDt  Dt� Du  Du� Dv  Dv� Dw  Dw� DwٚDy��D�9�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�
=A�A!�AA�Aa�A�(�A���A���A���A���A���A�(�A���B z�Bz�Bz�Bz�B z�B(z�B0z�B8z�B@z�BHz�BPz�BXz�B`�GBh�GBpz�Bxz�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�=qB�=qB�=qB�=qB�=qB�=qB�=qC �C�C�C�C�C
�C�C�C�C�C�C�CC�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8C:�C<�C>�C@�CB8RCD�CF�CH�CJ�CLCN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�CfCh�Cj�Cl8RCn8RCp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C��C��C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C��C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�)C�)C�\C�\C�\C��C�\C�\C�\C�\C�\C��C�\C�)C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�)C�\C�\C�\C�\C�\C��C�\C�\C�\C�\C�\C��C�\C�\C�\C�\C�\C�\C�\C�\C�\D �D �HD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	D	��D
�D
��D�D��D�D��D�D��DD�DD�D�D�HDHD��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�HDHD��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$�D%�D%��D&�D&��D'�D'�D(�D(�HD)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3HD3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DCDC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DKDK�DL�DL��DM�DM��DN�DN��DOHDO�HDP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV�HDW�DW��DX�DX�DYDY�DZ�DZ�HD[HD[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da�Db�Db��DcDc��Dd�Dd��De�De��Df�Df��Dg�Dg��DhDh��Di�Di�DjDj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��DqHDq��Dr�Dr��DsDs�Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw�HDy�=D�=�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��TA��`A���A���A�  A���A�  A�  A�A�1A�1A�%A�
=A�
=A���A�A��A��A۝�A�A��A�oAו�A���AֶFA�~�A�S�A��TA�r�A�Q�A�K�A�;dA���A���AԾwA�|�A�n�Aә�A�=qA�G�AУ�AϾwA�r�A��A̓A̾wA�^5A�oA��#A˛�A���A�33AɮAɑhA�9XA�7LA�5?A�$�AľwA�^5A���A�n�A�A�  A�t�A��A�ȴA���A��^A���A��A�A�
=A�oA��A�C�A�7LA�VA�(�A�ĜA��DA�C�A���A��9A�;dA���A���A�-A�+A�ĜA��A�+A��A��FA��-A��yA��A��#A��jA�r�A��^A�ȴA�C�A���A��TA�t�A��A�VA���A��A��A��^A���A��!A�VA�/A�O�A���A���A�/A�A}XAyO�AuAs��Ar�Ao�FAl�yAj��Ah�HAh9XAg�#Ad{Aa7LA_C�A[t�AX�AUG�ASVAQdZAPM�AOVANE�AMdZAK�;AJVAH��AG��AGAE��ADz�AC�ACXABĜAA�mA@�DA?�
A>��A=;dA<M�A;O�A:�A8�RA7�hA4�A2��A05?A/33A.$�A+�A*bA)?}A(�HA'\)A$��A#��A"�A ��A�FA
=A�AAVA�+A-A�HA�9Ap�A�AVA�A�\A�A�9AZA�wA�HAZA��A��A��A�A �A^5AdZAG�A�HA=qA5?A��A&�A	�
A	`BA+A�A�;A;dA9XA+A V@�hs@��D@�1'@� �@���@�l�@��\@�C�@�V@�n�@���@���@���@�;d@�\@��@��`@�dZ@�
=@��T@�t�@�ȴ@�V@���@�-@�?}@���@�v�@�@��H@���@���@�J@�ƨ@�n�@��#@��@ϥ�@�r�@�&�@�/@�%@͑h@ͺ^@�1@ͩ�@ɲ-@�n�@�/@��@���@��@�@�n�@�j@���@�&�@��T@���@���@��;@�E�@�C�@�+@�ȴ@�E�@�x�@�V@��9@��D@�bN@��P@�;d@���@��!@�M�@�{@��#@�@�p�@�&�@�V@��9@� �@�l�@�@��!@�{@���@��@��@��#@�x�@�hs@�/@��@�b@��F@��H@�-@��h@�/@���@��D@�z�@�9X@���@��F@���@���@��@�\)@��y@���@��-@�x�@��u@�bN@�(�@���@��@�dZ@�n�@�hs@��@�r�@�bN@� �@���@�;d@�?}@�Ĝ@�1'@�K�@��@�~�@�J@�@��-@���@�X@�j@�1@�t�@��R@�ff@�{@��T@���@���@��7@��@�Q�@��m@��@���@��;@�ƨ@��F@�dZ@�"�@���@��y@�@�n�@�=q@�5?@���@�@�?}@��D@�I�@��@��F@�ff@�X@�O�@�&�@���@��@�z�@��@��D@��@��@��m@��P@�"�@���@��\@��+@�=q@���@��@��u@�Q�@���@��@�K�@�+@�o@�"�@�ȴ@��\@�$�@��-@�O�@�&�@��j@�r�@�Z@�  @���@��+@�n�@�n�@�^5@�{@��7@�7L@��@���@���@���@�~�@�V@��@�@��T@���@��-@�p�@�G�@�V@��j@�bN@��@��w@���@�t�@�dZ@�C�@�
=@���@��H@���@��!@�^5@��@�J@��T@��7@���@�I�@��@���@�dZ@�@��@���@��+@�=q@��@���@�p�@�?}@���@���@��D@�I�@�(�@��@�@~ȴ@}o @m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��TA��`A���A���A�  A���A�  A�  A�A�1A�1A�%A�
=A�
=A���A�A��A��A۝�A�A��A�oAו�A���AֶFA�~�A�S�A��TA�r�A�Q�A�K�A�;dA���A���AԾwA�|�A�n�Aә�A�=qA�G�AУ�AϾwA�r�A��A̓A̾wA�^5A�oA��#A˛�A���A�33AɮAɑhA�9XA�7LA�5?A�$�AľwA�^5A���A�n�A�A�  A�t�A��A�ȴA���A��^A���A��A�A�
=A�oA��A�C�A�7LA�VA�(�A�ĜA��DA�C�A���A��9A�;dA���A���A�-A�+A�ĜA��A�+A��A��FA��-A��yA��A��#A��jA�r�A��^A�ȴA�C�A���A��TA�t�A��A�VA���A��A��A��^A���A��!A�VA�/A�O�A���A���A�/A�A}XAyO�AuAs��Ar�Ao�FAl�yAj��Ah�HAh9XAg�#Ad{Aa7LA_C�A[t�AX�AUG�ASVAQdZAPM�AOVANE�AMdZAK�;AJVAH��AG��AGAE��ADz�AC�ACXABĜAA�mA@�DA?�
A>��A=;dA<M�A;O�A:�A8�RA7�hA4�A2��A05?A/33A.$�A+�A*bA)?}A(�HA'\)A$��A#��A"�A ��A�FA
=A�AAVA�+A-A�HA�9Ap�A�AVA�A�\A�A�9AZA�wA�HAZA��A��A��A�A �A^5AdZAG�A�HA=qA5?A��A&�A	�
A	`BA+A�A�;A;dA9XA+A V@�hs@��D@�1'@� �@���@�l�@��\@�C�@�V@�n�@���@���@���@�;d@�\@��@��`@�dZ@�
=@��T@�t�@�ȴ@�V@���@�-@�?}@���@�v�@�@��H@���@���@�J@�ƨ@�n�@��#@��@ϥ�@�r�@�&�@�/@�%@͑h@ͺ^@�1@ͩ�@ɲ-@�n�@�/@��@���@��@�@�n�@�j@���@�&�@��T@���@���@��;@�E�@�C�@�+@�ȴ@�E�@�x�@�V@��9@��D@�bN@��P@�;d@���@��!@�M�@�{@��#@�@�p�@�&�@�V@��9@� �@�l�@�@��!@�{@���@��@��@��#@�x�@�hs@�/@��@�b@��F@��H@�-@��h@�/@���@��D@�z�@�9X@���@��F@���@���@��@�\)@��y@���@��-@�x�@��u@�bN@�(�@���@��@�dZ@�n�@�hs@��@�r�@�bN@� �@���@�;d@�?}@�Ĝ@�1'@�K�@��@�~�@�J@�@��-@���@�X@�j@�1@�t�@��R@�ff@�{@��T@���@���@��7@��@�Q�@��m@��@���@��;@�ƨ@��F@�dZ@�"�@���@��y@�@�n�@�=q@�5?@���@�@�?}@��D@�I�@��@��F@�ff@�X@�O�@�&�@���@��@�z�@��@��D@��@��@��m@��P@�"�@���@��\@��+@�=q@���@��@��u@�Q�@���@��@�K�@�+@�o@�"�@�ȴ@��\@�$�@��-@�O�@�&�@��j@�r�@�Z@�  @���@��+@�n�@�n�@�^5@�{@��7@�7L@��@���@���@���@�~�@�V@��@�@��T@���@��-@�p�@�G�@�V@��j@�bN@��@��w@���@�t�@�dZ@�C�@�
=@���@��H@���@��!@�^5@��@�J@��T@��7@���@�I�@��@���@�dZ@�@��@���@��+@�=q@��@���@�p�@�?}@���@���@��D@�I�@�(�@��@�@~ȴ@}o @m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
W
B
W
B
T�B
S�B
P�B
K�B
G�B
E�B
D�B
C�B
F�B
J�B
L�B
C�B
F�B
N�B
VB
W
B
VB
{�B
��B
�B�B0!BD�BXBk�B�%B�\B��B�B�jB��BƨB��B��B�BB�B��B��B��B�B:^BD�BS�BXBcTBjBv�B�hB�?B�?B�XB�wBƨBɺBȴB��B��BɺB��BȴBÖB�^B�-B��B}�BP�B>wB,B�B
=B��B�B�#B��B�B��B�7Bz�BdZBcTBk�BjBhsBdZB[#B]/BQ�B8RB.B&�B�BbB
=B
��B
�yB
�/B
��B
�qB
�3B
��B
��B
�{B
�JB
x�B
\)B
H�B
-B
bB
B	��B	�mB	��B	ƨB	�dB	�FB	�'B	��B	�=B	|�B	ffB	T�B	A�B	5?B	-B	%�B	�B	�B	�B	VB	B��B��B��B�B�B�sB�`B�ZB�BB�)B�B�B��B��B��B��B��BĜB�FB�-B�B��B��B��B�{B�bB�\B�JB�B}�Bz�Bw�Bw�Bu�Bu�Bv�B�B�B�B�Bs�Bo�Bo�Bw�B�7B�{B��B�B�B�B�LBŢB��B��B��BƨBƨB�}B�}B��B�wB�}BŢBɺBÖBĜB��B��BǮBĜB��B�^B�FB�?B�9B�3B�3B�3B�-B�!B�B��B��B��B��B�B�'B�3B�3B�9B�FB�LB�RB�XB�wB�}B��B��B��B��B��B��B�wB�?B��B��B��B��B�hB�\B�JB�DB�oB��B��B��B��B�9BÖB�jB�B��B�VB�hB��B��B�9B�FB�'B�3BŢBɺB�}B�RB��B�B��B��B��B��B	B	B	1B		7B		7B	PB	VB	bB	{B	�B	�B	�B	�B	�B	�B	 �B	"�B	%�B	'�B	)�B	.B	49B	7LB	9XB	:^B	<jB	@�B	@�B	A�B	C�B	H�B	I�B	M�B	P�B	T�B	XB	YB	[#B	\)B	]/B	`BB	`BB	`BB	`BB	aHB	aHB	cTB	hsB	jB	k�B	q�B	s�B	s�B	w�B	y�B	y�B	�B	�B	�B	�+B	�1B	�=B	�=B	�DB	�=B	�DB	�VB	�bB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�9B	�LB	�RB	�RB	�XB	�dB	�qB	�wB	��B	ÖB	ŢB	ƨB	ƨB	ƨB	ȴB	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�#B	�#B	�/B	�5B	�HB	�NB	�TB	�NB	�NB	�NB	�ZB	�`B	�ZB	�fB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B
B
B
B
  B	��B	��B	��B	��B
B
B
B
B
B
B
  B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
	7B

=B

=B

=B

=B

=B
DB
DB
DB
DB
JB
PB
PB
VB
VB
VB
VB
\B
bB
hB
oB
oB
oB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
)B
+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
W
B
W
B
T�B
S�B
P�B
K�B
G�B
E�B
D�B
C�B
F�B
J�B
L�B
C�B
F�B
N�B
VB
W
B
VB
{�B
��B
�B�B0!BD�BXBk�B�%B�\B��B�B�jB��BƨB��B��B�BB�B��B��B��B�B:^BD�BS�BXBcTBjBv�B�hB�?B�?B�XB�wBƨBɺBȴB��B��BɺB��BȴBÖB�^B�-B��B}�BP�B>wB,B�B
=B��B�B�#B��B�B��B�7Bz�BdZBcTBk�BjBhsBdZB[#B]/BQ�B8RB.B&�B�BbB
=B
��B
�yB
�/B
��B
�qB
�3B
��B
��B
�{B
�JB
x�B
\)B
H�B
-B
bB
B	��B	�mB	��B	ƨB	�dB	�FB	�'B	��B	�=B	|�B	ffB	T�B	A�B	5?B	-B	%�B	�B	�B	�B	VB	B��B��B��B�B�B�sB�`B�ZB�BB�)B�B�B��B��B��B��B��BĜB�FB�-B�B��B��B��B�{B�bB�\B�JB�B}�Bz�Bw�Bw�Bu�Bu�Bv�B�B�B�B�Bs�Bo�Bo�Bw�B�7B�{B��B�B�B�B�LBŢB��B��B��BƨBƨB�}B�}B��B�wB�}BŢBɺBÖBĜB��B��BǮBĜB��B�^B�FB�?B�9B�3B�3B�3B�-B�!B�B��B��B��B��B�B�'B�3B�3B�9B�FB�LB�RB�XB�wB�}B��B��B��B��B��B��B�wB�?B��B��B��B��B�hB�\B�JB�DB�oB��B��B��B��B�9BÖB�jB�B��B�VB�hB��B��B�9B�FB�'B�3BŢBɺB�}B�RB��B�B��B��B��B��B	B	B	1B		7B		7B	PB	VB	bB	{B	�B	�B	�B	�B	�B	�B	 �B	"�B	%�B	'�B	)�B	.B	49B	7LB	9XB	:^B	<jB	@�B	@�B	A�B	C�B	H�B	I�B	M�B	P�B	T�B	XB	YB	[#B	\)B	]/B	`BB	`BB	`BB	`BB	aHB	aHB	cTB	hsB	jB	k�B	q�B	s�B	s�B	w�B	y�B	y�B	�B	�B	�B	�+B	�1B	�=B	�=B	�DB	�=B	�DB	�VB	�bB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�9B	�LB	�RB	�RB	�XB	�dB	�qB	�wB	��B	ÖB	ŢB	ƨB	ƨB	ƨB	ȴB	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�#B	�#B	�/B	�5B	�HB	�NB	�TB	�NB	�NB	�NB	�ZB	�`B	�ZB	�fB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B
B
B
B
  B	��B	��B	��B	��B
B
B
B
B
B
B
  B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
	7B

=B

=B

=B

=B

=B
DB
DB
DB
DB
JB
PB
PB
VB
VB
VB
VB
\B
bB
hB
oB
oB
oB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
)B
+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.12 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140816                              AO  ARCAADJP                                                                    20181024140816    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140816  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140816  QCF$                G�O�G�O�G�O�0               