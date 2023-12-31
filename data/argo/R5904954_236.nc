CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:43Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191743  20181005191743  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d�~�P1   @��e8㠖@4����F�d���+1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb�Cd�Cf�Ch�Cj  Cl  Cn�Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C��C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C��3C�  C��3C��3C�  C�  C��C��C��C��C��C��C�  C�  C�  C��3C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C��3C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C��C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C��3C�  C��C��C�  C��3C��3C��3C��3C��fC��3C�  C��C��C�  C��C��C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  D fD � D ��D�fD��D�fD  D� D  D� D  D� D  D� D  D� D��D� D	fD	� D
fD
�fD  Dy�D  Dy�D��Dy�D��D� D  Dy�DfDy�D  Dy�DfD�fDfDy�D��D�fDfDy�D��D�fD  D� D  D� D  Dy�D  D� D  D� D��D�fD  D� DfDy�D  D� D   D � D!  D!� D"  D"y�D#  D#�fD$  D$� D%fD%� D&  D&�fD'  D'� D'��D(�fD)  D)� D*  D*� D+  D+� D+��D,� D-fD-y�D.fD.� D/  D/� D0  D0y�D1  D1y�D2  D2� D3  D3� D4  D4y�D5fD5� D6  D6� D6��D7� D8  D8� D9  D9� D:  D:�fD;  D;y�D<  D<� D=  D=� D>  D>y�D?fD?�fD?��D@� DAfDA� DB  DB� DC  DC� DDfDD� DE  DE� DFfDF� DF��DG� DH  DH�fDI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DM��DN�fDOfDOy�DO��DP�fDQ  DQ� DR  DR� DR��DS� DT  DTy�DUfDUy�DV  DV� DW  DW� DX  DX�fDY  DY� DZ  DZ�fD[  D[�fD[��D\� D]  D]y�D^  D^�fD^��D_y�D_��D`� D`��Da� Da��Db� Dc  Dc�fDd  Dd� De  De� DffDf� DgfDg� DhfDhy�DifDi� Dj  Dj� DkfDk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Do��Dp� Dq  Dq�fDr  Dr� Dr��Ds� Dt  Dt� Du  Du�fDv  Dv� Dw  Dwl�Dyj=D�;3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A��A$��AD��Ad��A�G�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB	=qB=qB=qB!=qB)=qB1=qB9=qBA=qBI��BQ=qBY=qBa=qBi=qBq=qBy=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B̞�BО�BԞ�B؞�Bܞ�B���B䞸B螸B잸B�B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\CO\CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&O\C(5�C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`h�Cbh�Cdh�Cfh�Chh�CjO\ClO\Cnh�CpO\Cr5�CtO\CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�4{C�4{C�'�C�4{C�'�C�'�C�4{C�4{C�4{C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C��C��C�'�C�'�C�4{C�4{C�4{C�4{C�4{C�4{C�'�C�'�C�'�C��C��C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C��C��C��C��C��C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�4{C�4{C�'�C�'�C�4{C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�4{C�4{C�'�C��C��C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C��C��C��C�'�C�4{C�4{C�'�C��C��C��C��C�C��C�'�C�4{C�4{C�'�C�4{C�4{C�4{C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�D =D ��DqD�=DqD�=D�D��D�D��D�D��D�D��D�D��DqD��D	=D	��D
=D
�=D�D�qD�D�qDqD�qDqD��D�D�qD=D�qD�D�qD=D�=D=D�qDqD�=D=D�qDqD�=D�D��D�D��D�D�qD�D��D�D��DqD�=D�D��D=D�qD�D��D �D ��D!�D!��D"�D"�qD#�D#�=D$�D$��D%=D%��D&�D&�=D'�D'��D(qD(�=D)�D)��D*�D*��D+�D+��D,qD,��D-=D-�qD.=D.��D/�D/��D0�D0�qD1�D1�qD2�D2��D3�D3��D4�D4�qD5=D5��D6�D6��D7qD7��D8�D8��D9�D9��D:�D:�=D;�D;�qD<�D<��D=�D=��D>�D>�qD?=D?�=D@qD@��DA=DA��DB�DB��DC�DC��DD=DD��DE�DE��DF=DF��DGqDG��DH�DH�=DI�DI��DJ�DJ��DK�DK��DL�DL��DM=DM��DNqDN�=DO=DO�qDPqDP�=DQ�DQ��DR�DR��DSqDS��DT�DT�qDU=DU�qDV�DV��DW�DW��DX�DX�=DY�DY��DZ�DZ�=D[�D[�=D\qD\��D]�D]�qD^�D^�=D_qD_�qD`qD`��DaqDa��DbqDb��Dc�Dc�=Dd�Dd��De�De��Df=Df��Dg=Dg��Dh=Dh�qDi=Di��Dj�Dj��Dk=Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do=Do��DpqDp��Dq�Dq�=Dr�Dr��DsqDs��Dt�Dt��Du�Du�=Dv�Dv��Dw�Dw��Dy~D�ED���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��`A��A��A��A��A��A��HA���Aț�A��A� �AƗ�A�33A���AŅA�(�A��Aĺ^A�~�A�S�A�+A��AøRAËDA�bA���A�ZA���A�p�A�hsA�XA��A��A��A��RA��^A��A��A��!A��7A�9XA�hsA�{A�ȴA���A��yA���A�bNA��yA��^A��A��`A��+A�  A�oA��#A�A�ƨA�bNA�
=A��^A�O�A��A�n�A�M�A���A��A��9A��wA��jA��/A���A�|�A��A��7A��mA�&�A�bNA�G�A��A��A�t�A�
=A�x�A�"�A���A���A�dZA�=qA�$�A�
=A��
A�&�A���A�l�A�ZA��A���A�A�O�A�ĜA�bNA�%A���A�oA�dZA�&�A�  A��A�%A}�7A{�Ay�TAvĜAvAtM�Ar��Aq"�Ap1AnJAl�Ak%Ai��Ag%AeO�Ac��Aa��A_�A_K�A^n�A\��A\5?AZ��AXĜAV�uATI�AR�/AQ�7AP�APE�AM��AJz�AG�#AF��AE��AB�A@z�A?l�A>��A=�wA;l�A7/A6M�A5�-A4��A3��A2VA0�+A/�FA/��A//A.E�A-?}A+�A*�A)�A)XA'&�A#\)A!��A �HA -AQ�A�HA5?A�Ax�AȴA��Ar�A��AjA1A�-A\)A~�A��A��AoA�AG�A~�A�-A�TA|�A
�HA
bA	��A	��A	�A	t�A	+A~�A��A�A�!A��A�\Az�A{A��AC�A~�A�#A�A �AO�A/AVA �A �`A �RA @�n�@�5?@���@�&�@��@��y@���@���@���@�j@�C�@�@��T@�S�@�@��@�Q�@�h@�@�5?@�Z@�-@ݙ�@�Ĝ@���@�@�v�@�?}@�+@պ^@���@�@�C�@�$�@��
@Ǖ�@��@���@ÍP@�33@�@��@�~�@�M�@�@���@�Z@�
=@��+@�^5@��@�@��7@�hs@�V@���@�S�@�$�@�@�X@�bN@�(�@�ƨ@���@�n�@�@�&�@�Ĝ@�9X@���@��P@�+@�
=@���@��@��!@��@�-@�v�@��@�=q@��-@��-@��@�J@�v�@�^5@�@���@��h@�hs@��@��9@�A�@��P@���@���@�O�@��@��@���@�(�@�(�@� �@�1@��
@���@��@��P@��P@�dZ@�o@�ȴ@��\@��@��7@�?}@�Z@���@�ƨ@�ƨ@��@�l�@�;d@���@��+@�@�?}@���@�$�@���@�X@��@�"�@�
=@���@�33@��P@�`B@�@��^@���@�hs@�G�@��@��@�?}@��@�  @���@�t�@�C�@���@�@�Q�@��w@�K�@�v�@��@���@��@��@�p�@��j@�r�@�A�@�(�@�b@���@��@�t�@�S�@��y@�ȴ@���@��R@��\@�v�@�E�@�-@�@���@��-@��7@�O�@��@��@���@���@�  @��;@���@��@�t�@�K�@��@��@���@�ȴ@���@��!@�~�@�V@�E�@�-@�{@���@�@��@��`@��j@��j@�Ĝ@��j@��9@���@�I�@��@�1@���@���@��m@���@���@��@��@�S�@��R@��y@��H@���@��!@�n�@�E�@�J@���@��@��/@��9@��D@��D@�j@�(�@�  @��@��@���@��P@�l�@�dZ@�K�@�"�@���@�n�@�=q@�M�@��^@�G�@���@�Ĝ@��j@���@�z�@�bN@���@��;@���@��F@�|�@�dZ@�C�@���@s�	@dM11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��`A��A��A��A��A��A��HA���Aț�A��A� �AƗ�A�33A���AŅA�(�A��Aĺ^A�~�A�S�A�+A��AøRAËDA�bA���A�ZA���A�p�A�hsA�XA��A��A��A��RA��^A��A��A��!A��7A�9XA�hsA�{A�ȴA���A��yA���A�bNA��yA��^A��A��`A��+A�  A�oA��#A�A�ƨA�bNA�
=A��^A�O�A��A�n�A�M�A���A��A��9A��wA��jA��/A���A�|�A��A��7A��mA�&�A�bNA�G�A��A��A�t�A�
=A�x�A�"�A���A���A�dZA�=qA�$�A�
=A��
A�&�A���A�l�A�ZA��A���A�A�O�A�ĜA�bNA�%A���A�oA�dZA�&�A�  A��A�%A}�7A{�Ay�TAvĜAvAtM�Ar��Aq"�Ap1AnJAl�Ak%Ai��Ag%AeO�Ac��Aa��A_�A_K�A^n�A\��A\5?AZ��AXĜAV�uATI�AR�/AQ�7AP�APE�AM��AJz�AG�#AF��AE��AB�A@z�A?l�A>��A=�wA;l�A7/A6M�A5�-A4��A3��A2VA0�+A/�FA/��A//A.E�A-?}A+�A*�A)�A)XA'&�A#\)A!��A �HA -AQ�A�HA5?A�Ax�AȴA��Ar�A��AjA1A�-A\)A~�A��A��AoA�AG�A~�A�-A�TA|�A
�HA
bA	��A	��A	�A	t�A	+A~�A��A�A�!A��A�\Az�A{A��AC�A~�A�#A�A �AO�A/AVA �A �`A �RA @�n�@�5?@���@�&�@��@��y@���@���@���@�j@�C�@�@��T@�S�@�@��@�Q�@�h@�@�5?@�Z@�-@ݙ�@�Ĝ@���@�@�v�@�?}@�+@պ^@���@�@�C�@�$�@��
@Ǖ�@��@���@ÍP@�33@�@��@�~�@�M�@�@���@�Z@�
=@��+@�^5@��@�@��7@�hs@�V@���@�S�@�$�@�@�X@�bN@�(�@�ƨ@���@�n�@�@�&�@�Ĝ@�9X@���@��P@�+@�
=@���@��@��!@��@�-@�v�@��@�=q@��-@��-@��@�J@�v�@�^5@�@���@��h@�hs@��@��9@�A�@��P@���@���@�O�@��@��@���@�(�@�(�@� �@�1@��
@���@��@��P@��P@�dZ@�o@�ȴ@��\@��@��7@�?}@�Z@���@�ƨ@�ƨ@��@�l�@�;d@���@��+@�@�?}@���@�$�@���@�X@��@�"�@�
=@���@�33@��P@�`B@�@��^@���@�hs@�G�@��@��@�?}@��@�  @���@�t�@�C�@���@�@�Q�@��w@�K�@�v�@��@���@��@��@�p�@��j@�r�@�A�@�(�@�b@���@��@�t�@�S�@��y@�ȴ@���@��R@��\@�v�@�E�@�-@�@���@��-@��7@�O�@��@��@���@���@�  @��;@���@��@�t�@�K�@��@��@���@�ȴ@���@��!@�~�@�V@�E�@�-@�{@���@�@��@��`@��j@��j@�Ĝ@��j@��9@���@�I�@��@�1@���@���@��m@���@���@��@��@�S�@��R@��y@��H@���@��!@�n�@�E�@�J@���@��@��/@��9@��D@��D@�j@�(�@�  @��@��@���@��P@�l�@�dZ@�K�@�"�@���@�n�@�=q@�M�@��^@�G�@���@�Ĝ@��j@���@�z�@�bN@���@��;@���@��F@�|�@�dZ@�C�@���@s�	@dM11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�#B�)B�)B�BB�`B�B%B/B@�BE�BH�BM�BR�BXB]/BbNBe`BhsBm�Br�Bu�B�B�%B�PB��B�9B��B��B��B�BB�`B�mB�ZB�)B�
B��B��B��BȴBǮBĜB�dB��B��B��B��B��B��B��B��B��B��B��B�oB�PB�DB�=B�Bm�BhsBZBM�BH�BI�BL�BM�BO�B>wB-B�B+B��B�ZB��B�XB��B��B�bB�VB�JB�B~�Bo�BXBP�BM�BJ�BF�B>wB\B
�B
�B
�)B
ɺB
�^B
�3B
�RB
�XB
�FB
�?B
�-B
�B
��B
��B
��B
��B
�\B
~�B
r�B
ffB
T�B
M�B
A�B
6FB
+B
 �B
�B
1B
  B	��B	�BB	��B	��B	�9B	��B	��B	��B	�{B	��B	�hB	�B	w�B	m�B	bNB	[#B	W
B	P�B	C�B	2-B	"�B	�B	{B	B��B��B�B�B�TB��B��B��B��B��BǮBÖBBŢBƨBĜBÖBÖB��B��B�jB�XB�!B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�oB�oB�bB�\B�PB�DB�7B�1B�1B�%B�B�B�B�B�B� B� B� B� B~�B}�B}�B}�B}�B}�B}�B|�B|�B{�B{�B{�Bz�By�By�Bz�Bz�Bz�Bz�Bz�By�By�Bz�Bz�By�By�By�By�By�Bx�Bx�By�Bw�Bx�By�B{�B}�B|�B{�B�B�B�B�B�B�B�B�B�B�B�%B�1B�PB�bB�\B�=B�1B�%B�%B�1B�DB�JB�PB�PB�VB�bB�bB�hB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�-B�FB�FB�^B�wB�wB�}B��BĜBƨBǮB��B�B�BB�ZB�sB�B��B	B	
=B	PB	oB	�B	�B	�B	�B	!�B	$�B	+B	,B	-B	/B	/B	2-B	6FB	6FB	6FB	7LB	8RB	8RB	9XB	:^B	:^B	;dB	>wB	B�B	D�B	F�B	L�B	M�B	P�B	R�B	T�B	T�B	T�B	W
B	XB	YB	YB	ZB	XB	R�B	T�B	YB	YB	T�B	VB	W
B	YB	_;B	cTB	t�B	y�B	z�B	|�B	~�B	~�B	~�B	|�B	{�B	}�B	|�B	~�B	�B	�B	�B	�B	~�B	�B	�B	�B	�B	�B	�+B	�VB	�\B	�\B	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�9B	�9B	�?B	�FB	�XB	�^B	�^B	�dB	�qB	�wB	�}B	��B	ĜB	ŢB	ƨB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�/B	�5B	�5B	�5B	�;B	�BB	�HB	�HB	�HB	�TB	�ZB	�`B	�`B	�fB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
�B
�B
'822222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B�B�B�B�#B�)B�)B�BB�`B�B%B/B@�BE�BH�BM�BR�BXB]/BbNBe`BhsBm�Br�Bu�B�B�%B�PB��B�9B��B��B��B�BB�`B�mB�ZB�)B�
B��B��B��BȴBǮBĜB�dB��B��B��B��B��B��B��B��B��B��B��B�oB�PB�DB�=B�Bm�BhsBZBM�BH�BI�BL�BM�BO�B>wB-B�B+B��B�ZB��B�XB��B��B�bB�VB�JB�B~�Bo�BXBP�BM�BJ�BF�B>wB\B
�B
�B
�)B
ɺB
�^B
�3B
�RB
�XB
�FB
�?B
�-B
�B
��B
��B
��B
��B
�\B
~�B
r�B
ffB
T�B
M�B
A�B
6FB
+B
 �B
�B
1B
  B	��B	�BB	��B	��B	�9B	��B	��B	��B	�{B	��B	�hB	�B	w�B	m�B	bNB	[#B	W
B	P�B	C�B	2-B	"�B	�B	{B	B��B��B�B�B�TB��B��B��B��B��BǮBÖBBŢBƨBĜBÖBÖB��B��B�jB�XB�!B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�oB�oB�bB�\B�PB�DB�7B�1B�1B�%B�B�B�B�B�B� B� B� B� B~�B}�B}�B}�B}�B}�B}�B|�B|�B{�B{�B{�Bz�By�By�Bz�Bz�Bz�Bz�Bz�By�By�Bz�Bz�By�By�By�By�By�Bx�Bx�By�Bw�Bx�By�B{�B}�B|�B{�B�B�B�B�B�B�B�B�B�B�B�%B�1B�PB�bB�\B�=B�1B�%B�%B�1B�DB�JB�PB�PB�VB�bB�bB�hB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�-B�FB�FB�^B�wB�wB�}B��BĜBƨBǮB��B�B�BB�ZB�sB�B��B	B	
=B	PB	oB	�B	�B	�B	�B	!�B	$�B	+B	,B	-B	/B	/B	2-B	6FB	6FB	6FB	7LB	8RB	8RB	9XB	:^B	:^B	;dB	>wB	B�B	D�B	F�B	L�B	M�B	P�B	R�B	T�B	T�B	T�B	W
B	XB	YB	YB	ZB	XB	R�B	T�B	YB	YB	T�B	VB	W
B	YB	_;B	cTB	t�B	y�B	z�B	|�B	~�B	~�B	~�B	|�B	{�B	}�B	|�B	~�B	�B	�B	�B	�B	~�B	�B	�B	�B	�B	�B	�+B	�VB	�\B	�\B	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�9B	�9B	�?B	�FB	�XB	�^B	�^B	�dB	�qB	�wB	�}B	��B	ĜB	ŢB	ƨB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�/B	�5B	�5B	�5B	�;B	�BB	�HB	�HB	�HB	�TB	�ZB	�`B	�`B	�fB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
�B
�B
'822222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191743                              AO  ARCAADJP                                                                    20181005191743    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191743  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191743  QCF$                G�O�G�O�G�O�8000            