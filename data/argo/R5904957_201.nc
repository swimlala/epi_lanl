CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:42Z creation      
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
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  Sx   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  f   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181024140842  20181024140842  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @���6l�51   @����Q�B@5<�1&��d
^5?|�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�33B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@�CB�CD�CF  CG�fCI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D ��D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D��D� D  Dy�D  D� D  D� D  D� D  Dy�D��Dy�D  D� D  D� D  Dy�D��D� D  D�fDfD� D  D� D��D� D  D� D  D�fD   D � D!  D!� D"fD"�fD#  D#� D$  D$� D%  D%� D&  D&y�D&��D'y�D'��D(� D)  D)� D*  D*�fD+  D+� D,  D,�fD-  D-y�D.  D.� D/  D/� D0  D0y�D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;y�D<  D<�fD=fD=� D>  D>y�D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DP� DQ  DQ� DQ��DR� DS  DSy�DT  DT� DU  DU� DV  DVy�DW  DW� DX  DX� DYfDY�fDZ  DZy�D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Db��Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl��Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq�fDr  Dr� Ds  Ds� Dt  Dt� DufDu�fDv  Dv� Dw  Dw� Dx  DxS3Dy� D�C�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��A��A$��AD��Ad��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB	=qB=qB=qB!=qB)=qB1=qB9=qBA=qBI=qBQ=qBY=qBa=qBi=qBq=qBy=qB�k�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B̞�B���BԞ�B�k�B�k�B���B䞸B螸B잸B�B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\CO\CO\CO\CO\CO\CO\CO\CO\C h�C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C85�C:O\C<O\C>O\C@h�CBh�CDh�CFO\CH5�CJ5�CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\CrO\Ct5�CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�4{C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D ��DqD��D=D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��DqD��D�D�qD�D��D�D��D�D��D�D�qDqD�qD�D��D�D��D�D�qDqD��D�D�=D=D��D�D��DqD��D�D��D�D�=D �D ��D!�D!��D"=D"�=D#�D#��D$�D$��D%�D%��D&�D&�qD'qD'�qD(qD(��D)�D)��D*�D*�=D+�D+��D,�D,�=D-�D-�qD.�D.��D/�D/��D0�D0�qD1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;�qD<�D<�=D==D=��D>�D>�qD?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DP��DQ�DQ��DRqDR��DS�DS�qDT�DT��DU�DU��DV�DV�qDW�DW��DX�DX��DY=DY�=DZ�DZ�qD[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��DcqDc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��DmqDm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq�=Dr�Dr��Ds�Ds��Dt�Dt��Du=Du�=Dv�Dv��Dw�Dw��Dx�Dxg
Dy��D�M�D�߯11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�|�A͇+A͇+A�|�A͇+A͇+ÁA�~�ÁÁA�~�A�x�A�|�A�z�A͍PA͏\A͗�A͓uA���A�=qA�A�A�
=A��
AʮAʋDA�p�A�1'A�VA��
A�x�A��
A�
=A�ffA�7LA��A��
A���A�ĜA�Q�A�XA�VA�\)A�;dA��A��A��\A�p�A�K�A��FA�bNA�(�A�n�A�t�A���A��PA���A���A��/A�+A��DA�;dA�ZA�n�A��A��hA��A�C�A��A��A��A���A�jA��A��jA���A�$�A�1A�ZA���A�\)A� �A��+A�+A��PA��uA��A�C�A��9A�$�A�I�A�x�A��^A�C�A�XA��A��A��A��7A�ȴA��^A�A�A�(�A��yA��A���A���A���A���A+A}��A{�Aw
=Ap�uAmC�Ah�yAf��Ae+Ab9XA`�uA_O�A^ �A]"�A[l�AZ^5AY%AW�PAVȴAU��ATffASVAP��AM/AKO�AJ-AG�AF{AC�AA`BA@A�A>1A=�A;�A:v�A:�A;K�A:ȴA:(�A9�A8��A8M�A8JA7�
A5�;A3��A3t�A2�A1�A0v�A.�uA-C�A,�!A,-A,  A+��A)�#A(�`A(~�A&��A$5?A"r�A"-A!��A �A 1A�PA��A�hA�yA�9AbNA��A��A�uAjA�
A|�A�A��A�AA��A=qA�A��AJA�FA;dA�7A	C�AĜA��A&�A  A�AƨA ȴ@��y@��@�
=@��D@�=q@�-@�X@��j@���@���@�ƨ@�o@�R@��@�u@�w@�l�@���@��T@�%@�I�@��@�^5@�^@�x�@�X@�u@�P@���@�ff@�A�@�dZ@�-@�\)@��@ޗ�@�$�@���@��@� �@�l�@ڇ+@��@�`B@���@�t�@���@Ցh@���@�\)@ҟ�@���@Ͼw@���@�7L@��@�?}@��#@���@�$�@��/@ț�@�b@���@��@�%@��`@Ĭ@�I�@��@���@+@�9X@�
=@��
@��
@�`B@��@�x�@�O�@��@��;@��y@�v�@�^5@�$�@��T@���@�@�x�@�%@�Q�@��@�ƨ@�|�@�S�@�+@��y@���@�E�@�-@�$�@��@���@�x�@���@�Q�@�C�@��@���@��
@�^5@��@��y@�
=@�r�@��@��T@�K�@�I�@�ƨ@��@��+@�~�@�@���@���@��7@��@�Q�@��P@�;d@�@�v�@��^@��@�p�@�hs@�X@�O�@��/@�I�@���@�dZ@�+@���@�ȴ@���@�v�@�=q@��@��@��#@���@�&�@��u@��@�C�@�o@��y@���@�V@���@��h@�`B@�?}@��@��@�V@��/@�bN@�9X@�1@��F@�K�@��H@���@���@���@���@��R@��!@���@�v�@�z�@��9@��u@��@�j@�A�@�(�@���@��w@�dZ@�K�@�C�@�C�@�C�@�"�@���@�M�@�-@�{@��@���@��7@�X@���@�I�@���@�"�@��y@���@�~�@�V@�E�@�J@���@��#@�x�@�O�@���@�r�@�I�@�A�@�1'@��@� �@�(�@� �@�b@�ƨ@�|�@�"�@�@��y@�ȴ@���@��\@��\@�~�@�n�@�{@��-@��7@�`B@�/@��`@�A�@��w@���@�t�@�K�@�K�@�33@�@�ȴ@��\@�^5@�-@���@���@�hs@�?}@��@���@���@x��@iVm11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�|�A͇+A͇+A�|�A͇+A͇+ÁA�~�ÁÁA�~�A�x�A�|�A�z�A͍PA͏\A͗�A͓uA���A�=qA�A�A�
=A��
AʮAʋDA�p�A�1'A�VA��
A�x�A��
A�
=A�ffA�7LA��A��
A���A�ĜA�Q�A�XA�VA�\)A�;dA��A��A��\A�p�A�K�A��FA�bNA�(�A�n�A�t�A���A��PA���A���A��/A�+A��DA�;dA�ZA�n�A��A��hA��A�C�A��A��A��A���A�jA��A��jA���A�$�A�1A�ZA���A�\)A� �A��+A�+A��PA��uA��A�C�A��9A�$�A�I�A�x�A��^A�C�A�XA��A��A��A��7A�ȴA��^A�A�A�(�A��yA��A���A���A���A���A+A}��A{�Aw
=Ap�uAmC�Ah�yAf��Ae+Ab9XA`�uA_O�A^ �A]"�A[l�AZ^5AY%AW�PAVȴAU��ATffASVAP��AM/AKO�AJ-AG�AF{AC�AA`BA@A�A>1A=�A;�A:v�A:�A;K�A:ȴA:(�A9�A8��A8M�A8JA7�
A5�;A3��A3t�A2�A1�A0v�A.�uA-C�A,�!A,-A,  A+��A)�#A(�`A(~�A&��A$5?A"r�A"-A!��A �A 1A�PA��A�hA�yA�9AbNA��A��A�uAjA�
A|�A�A��A�AA��A=qA�A��AJA�FA;dA�7A	C�AĜA��A&�A  A�AƨA ȴ@��y@��@�
=@��D@�=q@�-@�X@��j@���@���@�ƨ@�o@�R@��@�u@�w@�l�@���@��T@�%@�I�@��@�^5@�^@�x�@�X@�u@�P@���@�ff@�A�@�dZ@�-@�\)@��@ޗ�@�$�@���@��@� �@�l�@ڇ+@��@�`B@���@�t�@���@Ցh@���@�\)@ҟ�@���@Ͼw@���@�7L@��@�?}@��#@���@�$�@��/@ț�@�b@���@��@�%@��`@Ĭ@�I�@��@���@+@�9X@�
=@��
@��
@�`B@��@�x�@�O�@��@��;@��y@�v�@�^5@�$�@��T@���@�@�x�@�%@�Q�@��@�ƨ@�|�@�S�@�+@��y@���@�E�@�-@�$�@��@���@�x�@���@�Q�@�C�@��@���@��
@�^5@��@��y@�
=@�r�@��@��T@�K�@�I�@�ƨ@��@��+@�~�@�@���@���@��7@��@�Q�@��P@�;d@�@�v�@��^@��@�p�@�hs@�X@�O�@��/@�I�@���@�dZ@�+@���@�ȴ@���@�v�@�=q@��@��@��#@���@�&�@��u@��@�C�@�o@��y@���@�V@���@��h@�`B@�?}@��@��@�V@��/@�bN@�9X@�1@��F@�K�@��H@���@���@���@���@��R@��!@���@�v�@�z�@��9@��u@��@�j@�A�@�(�@���@��w@�dZ@�K�@�C�@�C�@�C�@�"�@���@�M�@�-@�{@��@���@��7@�X@���@�I�@���@�"�@��y@���@�~�@�V@�E�@�J@���@��#@�x�@�O�@���@�r�@�I�@�A�@�1'@��@� �@�(�@� �@�b@�ƨ@�|�@�"�@�@��y@�ȴ@���@��\@��\@�~�@�n�@�{@��-@��7@�`B@�/@��`@�A�@��w@���@�t�@�K�@�K�@�33@�@�ȴ@��\@�^5@�-@���@���@�hs@�?}@��@���@���@x��@iVm11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BD�BD�BD�BD�BD�BD�BD�BD�BD�BD�BD�BD�BD�BD�BD�BE�BJ�BO�B�PB�FB�'B�9B�FB�dB��B�sB��B1B�B�B2-B\)Bo�Bu�Bw�By�B�B�B�Be`B`BBn�BjBp�Bo�BdZBw�B�=B�JB�PB�VB�oB�\Bz�Bt�Bs�Bk�BgmBdZB_;BW
BQ�BM�BH�BD�BB�BA�B<jB;dB;dB7LB"�BbBB��B�B�NB�B��BƨB�qB��B�=Bo�B_;BW
BJ�B@�B8RB-B�BB
�B
��B
ĜB
�LB
�LB
�3B
��B
��B
�7B
z�B
t�B
x�B
|�B
}�B
�B
�B
�B
k�B
aHB
C�B
�B	�;B	ǮB	��B	��B	�1B	p�B	aHB	VB	L�B	C�B	7LB	/B	&�B	�B	�B	�B	�B	bB	B�B�/B�
B�)B��B��B�'B�B��B��B��B��B�RB��B��B��B��B�B�B��B��B��BɺBƨBB�qB�dB�jB�LB�9B�-B�'B�B��B��B��B�{B�%B� B}�B}�B~�B�B�%B�+B�7B�7B�1B�+B�%B�B}�By�Bx�Bw�Bv�Bt�B|�B~�B�B�B�B�B�B�B~�By�Bp�Bp�Bm�BaHB[#BXBVBS�BQ�BR�BP�BQ�BZBe`BjBl�Bt�B}�B�B�=B�JB�VB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�'B�-B�3B�9B�RB�jB�jB�qB�qB�qB�qB�LB�9B�LB�wB��BƨB��B��B��B�
B�B�#B�5B�NB�NB�NB�NB�NB�HB�B�}B�^B�wB�wB�FB��B��B��B��B��B�B�'B�'B�'B�-B�-B�-B�-B�9B�XB�dB�}BBÖBĜBŢBǮBɺB��B��B��B��B��B�
B�5B�B�B��B	JB	�B	$�B	&�B	+B	7LB	B�B	I�B	YB	e`B	e`B	dZB	gmB	m�B	r�B	k�B	iyB	l�B	n�B	q�B	u�B	y�B	y�B	|�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�JB	�\B	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�3B	�3B	�3B	�9B	�LB	�RB	�XB	�dB	�qB	�}B	��B	��B	��B	��B	B	B	ÖB	ĜB$�B	�B	�B	�B	�#B	�/B	�;B	�BB	�HB	�TB	�TB	�TB	�TB	�TB	�TB	�`B	�fB	�mB	�mB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
1B

=B
DB
JB
PB
PB
VB
\B
bB
bB
hB
hB
hB
hB
oB
�B
5B
/ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111BD�BD�BD�BD�BD�BD�BD�BD�BD�BD�BD�BD�BD�BD�BD�BE�BJ�BO�B�PB�FB�'B�9B�FB�dB��B�sB��B1B�B�B2-B\)Bo�Bu�Bw�By�B�B�B�Be`B`BBn�BjBp�Bo�BdZBw�B�=B�JB�PB�VB�oB�\Bz�Bt�Bs�Bk�BgmBdZB_;BW
BQ�BM�BH�BD�BB�BA�B<jB;dB;dB7LB"�BbBB��B�B�NB�B��BƨB�qB��B�=Bo�B_;BW
BJ�B@�B8RB-B�BB
�B
��B
ĜB
�LB
�LB
�3B
��B
��B
�7B
z�B
t�B
x�B
|�B
}�B
�B
�B
�B
k�B
aHB
C�B
�B	�;B	ǮB	��B	��B	�1B	p�B	aHB	VB	L�B	C�B	7LB	/B	&�B	�B	�B	�B	�B	bB	B�B�/B�
B�)B��B��B�'B�B��B��B��B��B�RB��B��B��B��B�B�B��B��B��BɺBƨBB�qB�dB�jB�LB�9B�-B�'B�B��B��B��B�{B�%B� B}�B}�B~�B�B�%B�+B�7B�7B�1B�+B�%B�B}�By�Bx�Bw�Bv�Bt�B|�B~�B�B�B�B�B�B�B~�By�Bp�Bp�Bm�BaHB[#BXBVBS�BQ�BR�BP�BQ�BZBe`BjBl�Bt�B}�B�B�=B�JB�VB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�'B�-B�3B�9B�RB�jB�jB�qB�qB�qB�qB�LB�9B�LB�wB��BƨB��B��B��B�
B�B�#B�5B�NB�NB�NB�NB�NB�HB�B�}B�^B�wB�wB�FB��B��B��B��B��B�B�'B�'B�'B�-B�-B�-B�-B�9B�XB�dB�}BBÖBĜBŢBǮBɺB��B��B��B��B��B�
B�5B�B�B��B	JB	�B	$�B	&�B	+B	7LB	B�B	I�B	YB	e`B	e`B	dZB	gmB	m�B	r�B	k�B	iyB	l�B	n�B	q�B	u�B	y�B	y�B	|�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�JB	�\B	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�3B	�3B	�3B	�9B	�LB	�RB	�XB	�dB	�qB	�}B	��B	��B	��B	��B	B	B	ÖB	ĜB$�B	�B	�B	�B	�#B	�/B	�;B	�BB	�HB	�TB	�TB	�TB	�TB	�TB	�TB	�`B	�fB	�mB	�mB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
1B

=B
DB
JB
PB
PB
VB
\B
bB
bB
hB
hB
hB
hB
oB
�B
5B
/ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140842                              AO  ARCAADJP                                                                    20181024140842    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140842  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140842  QCF$                G�O�G�O�G�O�0               