CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:07Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191707  20181005191707  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               TA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��dlw�1   @��d����@4��
=q�d2V�u1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      TA   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB��B��B��B'��B0  B8  B@  BH  BP  BXffB`ffBhffBp  Bx  B�33B�  B���B���B���B���B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���C  C  C  C  C	�fC  C�C  C  C  C  C  C�fC  C  C   C"  C$  C&  C(�C*  C+�fC.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CG�fCI�fCL  CN�CP  CR  CT  CV  CW�fCZ  C\�C^�C`�Cb  Cd  Cf  Cg�fCi�fCk�fCn  Cp  Cr  Ct  Cv  Cx33Cz�C|  C~  C�  C��3C��3C�  C��C��C�  C�  C�  C��3C��fC�  C��C�  C��3C�  C��C��3C�  C��C��3C�  C��C�  C��C��C�  C�  C�  C�  C��C�  C�  C��3C��C��3C�  C��C��fC��C��3C��C�  C��C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��3C��3C��C�  C��3C��C��C�  C��C�  C��3C�  C�  C��C��C�  C��C��C�  C��3C��3C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C��C��C��C�  C�  C�  C��3C��C��3C�  C�  C�  C�  C��C��C��C��C��C��C��C�  C�  C�  C��3D fD �fDfD�fDfD� D��Dy�D  D� D  D�fDfD� D  D�fDfD� D	  D	� D	��D
� DfDy�D  D� D  D�fD�D� D��D� DfD�fD��D� D��Dy�D��D� D  Dy�D��D�fDfD� D  D� D��Dy�D  D� DfD�fDfD� D  D� D  Dy�D��D� DfD�fD fD � D!  D!� D"  D"� D#  D#� D$fD$�fD%  D%� D&fD&� D'  D'� D'��D(� D)  D)� D*fD*� D+fD+� D,  D,� D,��D-� D.  D.� D/fD/� D0  D0� D1  D1�fD2  D2� D3fD3�fD4  D4y�D4��D5� D6  D6� D7  D7� D8  D8� D9  D9y�D9��D:y�D:��D;� D<  D<� D=  D=� D=��D>� D?  D?� D@  D@� DA  DA� DB  DB�fDC  DCy�DD  DD� DE  DE� DF  DF� DF��DGy�DG��DH� DI  DIy�DJ  DJ�fDKfDK� DLfDL� DL��DM� DN  DN� DOfDO� DP  DP� DQfDQ�fDR�DR�fDS  DSy�DT  DT�fDUfDU�fDV  DV� DV��DWy�DX  DXy�DY  DY� DY��DZy�D[  D[y�D[��D\y�D]  D]�fD^fD^� D_  D_� D`  D`� Da  Da� Db  Db�fDc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg�fDh  Dhy�Dh��Di� Dj  Djy�Dj��Dk� Dl  Dl� Dm  Dm�fDn  Dny�Dn��Do� Dp  Dp� Dq  Dqy�Dr  Dr� Dr��Ds� Dt  Dty�Dt��Du� DvfDv� Dv��Dws3Dy[�D�>�D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A��A$��AD��Ad��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB	��B
>B�B �B(�B1=qB9=qBA=qBI=qBQ=qBY��Ba��Bi��Bq=qBy=qB���B���B�k�B�k�B�k�B�k�B���B���B���B���B���B���B���B�k�B�k�B���B���BĞ�BȞ�B̞�BО�BԞ�B؞�Bܞ�B���B䞸B螸B잸B�B���B���B�k�C 5�CO\CO\CO\CO\C
5�CO\Ch�CO\CO\CO\CO\CO\C5�CO\CO\C O\C"O\C$O\C&O\C(h�C*O\C,5�C.O\C0h�C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CH5�CJ5�CLO\CNh�CPO\CRO\CTO\CVO\CX5�CZO\C\h�C^h�C`h�CbO\CdO\CfO\Ch5�Cj5�Cl5�CnO\CpO\CrO\CtO\CvO\Cx��Czh�C|O\C~O\C�'�C��C��C�'�C�4{C�4{C�'�C�'�C�'�C��C�C�'�C�4{C�'�C��C�'�C�4{C��C�'�C�4{C��C�'�C�4{C�'�C�4{C�4{C�'�C�'�C�'�C�'�C�4{C�'�C�'�C��C�4{C��C�'�C�4{C�C�4{C��C�4{C�'�C�4{C�'�C�'�C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�4{C��C��C�4{C�'�C��C�4{C�4{C�'�C�4{C�'�C��C�'�C�'�C�4{C�4{C�'�C�4{C�4{C�'�C��C��C�'�C�4{C�'�C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�4{C�4{C�4{C�'�C�'�C�'�C��C�4{C��C�'�C�'�C�'�C�'�C�4{C�4{C�4{C�4{C�4{C�4{C�AHC�'�C�'�C�'�C��D =D �=D=D�=D=D��DqD�qD�D��D�D�=D=D��D�D�=D=D��D	�D	��D
qD
��D=D�qD�D��D�D�=D �D��DqD��D=D�=DqD��DqD�qDqD��D�D�qDqD�=D=D��D�D��DqD�qD�D��D=D�=D=D��D�D��D�D�qDqD��D=D�=D =D ��D!�D!��D"�D"��D#�D#��D$=D$�=D%�D%��D&=D&��D'�D'��D(qD(��D)�D)��D*=D*��D+=D+��D,�D,��D-qD-��D.�D.��D/=D/��D0�D0��D1�D1�=D2�D2��D3=D3�=D4�D4�qD5qD5��D6�D6��D7�D7��D8�D8��D9�D9�qD:qD:�qD;qD;��D<�D<��D=�D=��D>qD>��D?�D?��D@�D@��DA�DA��DB�DB�=DC�DC�qDD�DD��DE�DE��DF�DF��DGqDG�qDHqDH��DI�DI�qDJ�DJ�=DK=DK��DL=DL��DMqDM��DN�DN��DO=DO��DP�DP��DQ=DQ�=DR �DR�=DS�DS�qDT�DT�=DU=DU�=DV�DV��DWqDW�qDX�DX�qDY�DY��DZqDZ�qD[�D[�qD\qD\�qD]�D]�=D^=D^��D_�D_��D`�D`��Da�Da��Db�Db�=Dc�Dc��Dd�Dd��De=De��Df�Df��Dg�Dg�=Dh�Dh�qDiqDi��Dj�Dj�qDkqDk��Dl�Dl��Dm�Dm�=Dn�Dn�qDoqDo��Dp�Dp��Dq�Dq�qDr�Dr��DsqDs��Dt�Dt�qDuqDu��Dv=Dv��DwqDw�
Dyo\D�H�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ƨA���A���A���A���A���A���A��A��A��A��A��
A��A��A��/AܾwA�`BA�l�AՓuAӗ�A�z�A�  A˅A��/A�$�A�G�A�bNA�
=A��
A�^5A�&�AìA�^5A�33A�/A�G�A��uA���A��
A�"�A�p�A�33A�{A���A���A���A�;dA�VA��hA�-A�=qA�G�A��HA��A��TA�&�A��A��HA��uA��/A�K�A���A���A�{A�  A�=qA��-A���A�I�A�v�A��RA�=qA�A�VA��FA�VA���A��A�7LA��A��7A�A��9A��TA�l�A�hsA�I�A�M�A�  A���A�O�A�ƨA�
=A�ȴA�-A�/A~�HA|�/Az��Ay�hAw�mAvZAt�Aq`BAohsAnbAk�Ah1'Ae�;Ae+Ad{Ab�A`z�A_K�A^M�A\��A[33AYoAU`BAT  ASdZAR�+AQAMdZAK�AJ�+AIG�AHbNAF��AFA�AD�AC��AC33AB�yABVA?l�A<�uA:JA9�PA9A81A6�HA5XA3�A1O�A/l�A.��A.{A-x�A,Q�A+�A*��A)�A(��A&A�A%��A%;dA$�A#XA"��A!+A �DA�^A��A�-AA��A�\A�DAjAS�A\)A�9A�AA�AƨAG�A�RA�A�yA��A��A/AĜAVA�^AK�A
�!A	��AA�jA�Al�AE�At�A �/A 9X@��;@�ƨ@�V@�l�@��9@��-@�I�@�{@���@��-@��@�F@�J@�7L@�1@�;d@���@�h@��@���@�C�@�~�@��#@�7L@�E�@�p�@��@��@ܓu@� �@���@�dZ@��@�@�ȴ@�v�@�M�@�5?@�$�@���@���@���@�"�@ҟ�@��@��#@�9X@ͺ^@�7L@�b@��@�V@��@ɲ-@�\)@ʟ�@�x�@�X@���@�5?@ċD@���@Õ�@��P@�5?@��@��@�hs@��@���@��@�-@�G�@�(�@�|�@�;d@�;d@�"�@��R@��+@�=q@��@��#@���@�G�@���@�33@�V@��@�
=@���@���@�`B@�^5@��#@��h@�  @���@�v�@�E�@��T@��7@���@���@��@��7@�E�@��@��!@���@��@�ƨ@���@�;d@��@��\@��@��@�(�@�;d@���@�n�@��h@�K�@��@�dZ@�5?@�hs@��^@���@���@�ff@�@�;d@�33@��R@�n�@��R@���@�"�@�"�@�K�@�$�@�J@�-@��@���@��7@�7L@��@��@��m@�ƨ@��P@�dZ@�C�@���@��@��w@��w@��w@���@�|�@�\)@��R@�ff@�=q@��@���@��h@�?}@�A�@�1@��w@�K�@��@���@�M�@�5?@�$�@�J@�@��@��/@�Ĝ@�bN@��;@�dZ@��@�
=@���@��H@���@�ȴ@���@��R@���@�~�@�v�@�M�@�{@�X@�V@�V@�V@���@���@�Ĝ@���@��D@�z�@�j@� �@��@�\)@�33@�"�@��R@�~�@�=q@��@���@�`B@�/@���@��/@�Ĝ@�Ĝ@��@��u@��u@��u@��u@��u@��u@��u@��D@��@�r�@�Z@���@��@�\)@�S�@�K�@�
=@�@��@���@�ȴ@�{@��T@���@��u@�ƨ@���@�C�@�@��@��@���@�ff@�-@��T@���@��-@���@�hs@�%@�Ĝ@�z�@�1'@��m@�ƨ@��F@��P@�S�@���@�=q@��#@���@���@��h@�p�@�/@���@���@�Ĝ@��9@���@��@�Q�@�1'@��
@�C�@�
=@���@���@wb�@^ȴ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ƨA���A���A���A���A���A���A��A��A��A��A��
A��A��A��/AܾwA�`BA�l�AՓuAӗ�A�z�A�  A˅A��/A�$�A�G�A�bNA�
=A��
A�^5A�&�AìA�^5A�33A�/A�G�A��uA���A��
A�"�A�p�A�33A�{A���A���A���A�;dA�VA��hA�-A�=qA�G�A��HA��A��TA�&�A��A��HA��uA��/A�K�A���A���A�{A�  A�=qA��-A���A�I�A�v�A��RA�=qA�A�VA��FA�VA���A��A�7LA��A��7A�A��9A��TA�l�A�hsA�I�A�M�A�  A���A�O�A�ƨA�
=A�ȴA�-A�/A~�HA|�/Az��Ay�hAw�mAvZAt�Aq`BAohsAnbAk�Ah1'Ae�;Ae+Ad{Ab�A`z�A_K�A^M�A\��A[33AYoAU`BAT  ASdZAR�+AQAMdZAK�AJ�+AIG�AHbNAF��AFA�AD�AC��AC33AB�yABVA?l�A<�uA:JA9�PA9A81A6�HA5XA3�A1O�A/l�A.��A.{A-x�A,Q�A+�A*��A)�A(��A&A�A%��A%;dA$�A#XA"��A!+A �DA�^A��A�-AA��A�\A�DAjAS�A\)A�9A�AA�AƨAG�A�RA�A�yA��A��A/AĜAVA�^AK�A
�!A	��AA�jA�Al�AE�At�A �/A 9X@��;@�ƨ@�V@�l�@��9@��-@�I�@�{@���@��-@��@�F@�J@�7L@�1@�;d@���@�h@��@���@�C�@�~�@��#@�7L@�E�@�p�@��@��@ܓu@� �@���@�dZ@��@�@�ȴ@�v�@�M�@�5?@�$�@���@���@���@�"�@ҟ�@��@��#@�9X@ͺ^@�7L@�b@��@�V@��@ɲ-@�\)@ʟ�@�x�@�X@���@�5?@ċD@���@Õ�@��P@�5?@��@��@�hs@��@���@��@�-@�G�@�(�@�|�@�;d@�;d@�"�@��R@��+@�=q@��@��#@���@�G�@���@�33@�V@��@�
=@���@���@�`B@�^5@��#@��h@�  @���@�v�@�E�@��T@��7@���@���@��@��7@�E�@��@��!@���@��@�ƨ@���@�;d@��@��\@��@��@�(�@�;d@���@�n�@��h@�K�@��@�dZ@�5?@�hs@��^@���@���@�ff@�@�;d@�33@��R@�n�@��R@���@�"�@�"�@�K�@�$�@�J@�-@��@���@��7@�7L@��@��@��m@�ƨ@��P@�dZ@�C�@���@��@��w@��w@��w@���@�|�@�\)@��R@�ff@�=q@��@���@��h@�?}@�A�@�1@��w@�K�@��@���@�M�@�5?@�$�@�J@�@��@��/@�Ĝ@�bN@��;@�dZ@��@�
=@���@��H@���@�ȴ@���@��R@���@�~�@�v�@�M�@�{@�X@�V@�V@�V@���@���@�Ĝ@���@��D@�z�@�j@� �@��@�\)@�33@�"�@��R@�~�@�=q@��@���@�`B@�/@���@��/@�Ĝ@�Ĝ@��@��u@��u@��u@��u@��u@��u@��u@��D@��@�r�@�Z@���@��@�\)@�S�@�K�@�
=@�@��@���@�ȴ@�{@��T@���@��u@�ƨ@���@�C�@�@��@��@���@�ff@�-@��T@���@��-@���@�hs@�%@�Ĝ@�z�@�1'@��m@�ƨ@��F@��P@�S�@���@�=q@��#@���@���@��h@�p�@�/@���@���@�Ĝ@��9@���@��@�Q�@�1'@��
@�C�@�
=@���@���@wb�@^ȴ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B(�B(�B'�B(�B'�B'�B'�B'�B(�B'�B'�B'�B'�B'�B'�B&�B!�BVB1B1B�B �B(�B,B.B49B;dB=qBE�BN�BS�BYB_;BiyBt�B�B�1B�\B�uB��B��B��B��B��B��B��B�{B�{B�oB�{B�oB�oB��B��B�VB� Bx�Bk�BffB^5BXBJ�B@�B6FB&�B�BoBB��B�B�NB�B��B��B�}B�?B��B�1Bn�B^5BJ�B@�B:^B-B�B
=B
��B
��B
�B
�/B
ɺB
ÖB
�B
��B
�bB
q�B
K�B
:^B
+B
�B
hB
B	�B	�/B	��B	��B	�9B	��B	�bB	�=B	�B	x�B	m�B	cTB	]/B	S�B	J�B	>wB	.B	$�B	!�B	�B	{B	+B��B��B��B�B�B�B�sB�TB�HB�;B�#B��B��BÖB��B�wB�^B�FB�'B�B��B��B��B��B��B��B��B��B��B�{B��B�uB�oB�oB�bB�hB�bB�bB�bB�\B�\B��B��B��B�{B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�VB�+B�B~�B� B� B�B�B�%B�B�B�B�B�+B�1B�1B�1B�%B�%B�1B�DB�=B�PB�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB��B��B�'B�3B�9B�LB�LB�9B�B�?B�'B��B�uB�uB��B�VB�JB�bB�uB��B��B��B��B��B��B��B��B�B�B�B�!B�9B�XB��B��BŢB��B�B�5B�;B�HB�B�B�B�B�B�B��B��B	B	1B	JB	\B	uB	�B	�B	#�B	.B	7LB	33B	/B	8RB	O�B	T�B	\)B	_;B	`BB	cTB	bNB	aHB	]/B	T�B	L�B	H�B	E�B	E�B	J�B	M�B	N�B	VB	]/B	_;B	_;B	^5B	_;B	cTB	jB	n�B	p�B	u�B	y�B	|�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�PB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�9B	�?B	�LB	�RB	�RB	�RB	�XB	�wB	�}B	��B	��B	ĜB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�)B	�/B	�HB	�TB	�ZB	�ZB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
%B
%B
%B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B
JB
JB
PB
PB
PB
PB
VB
VB
\B
\B
\B
\B
\B
\B
\B
\B
bB
hB
hB
oB
�B
xB
+�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B(�B(�B'�B(�B'�B'�B'�B'�B(�B'�B'�B'�B'�B'�B'�B&�B!�BVB1B1B�B �B(�B,B.B49B;dB=qBE�BN�BS�BYB_;BiyBt�B�B�1B�\B�uB��B��B��B��B��B��B��B�{B�{B�oB�{B�oB�oB��B��B�VB� Bx�Bk�BffB^5BXBJ�B@�B6FB&�B�BoBB��B�B�NB�B��B��B�}B�?B��B�1Bn�B^5BJ�B@�B:^B-B�B
=B
��B
��B
�B
�/B
ɺB
ÖB
�B
��B
�bB
q�B
K�B
:^B
+B
�B
hB
B	�B	�/B	��B	��B	�9B	��B	�bB	�=B	�B	x�B	m�B	cTB	]/B	S�B	J�B	>wB	.B	$�B	!�B	�B	{B	+B��B��B��B�B�B�B�sB�TB�HB�;B�#B��B��BÖB��B�wB�^B�FB�'B�B��B��B��B��B��B��B��B��B��B�{B��B�uB�oB�oB�bB�hB�bB�bB�bB�\B�\B��B��B��B�{B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�VB�+B�B~�B� B� B�B�B�%B�B�B�B�B�+B�1B�1B�1B�%B�%B�1B�DB�=B�PB�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB��B��B�'B�3B�9B�LB�LB�9B�B�?B�'B��B�uB�uB��B�VB�JB�bB�uB��B��B��B��B��B��B��B��B�B�B�B�!B�9B�XB��B��BŢB��B�B�5B�;B�HB�B�B�B�B�B�B��B��B	B	1B	JB	\B	uB	�B	�B	#�B	.B	7LB	33B	/B	8RB	O�B	T�B	\)B	_;B	`BB	cTB	bNB	aHB	]/B	T�B	L�B	H�B	E�B	E�B	J�B	M�B	N�B	VB	]/B	_;B	_;B	^5B	_;B	cTB	jB	n�B	p�B	u�B	y�B	|�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�PB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�9B	�?B	�LB	�RB	�RB	�RB	�XB	�wB	�}B	��B	��B	ĜB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�)B	�/B	�HB	�TB	�ZB	�ZB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
%B
%B
%B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B
JB
JB
PB
PB
PB
PB
VB
VB
\B
\B
\B
\B
\B
\B
\B
\B
bB
hB
hB
oB
�B
xB
+�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191707                              AO  ARCAADJP                                                                    20181005191707    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191707  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191707  QCF$                G�O�G�O�G�O�8000            