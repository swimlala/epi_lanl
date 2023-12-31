CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:07Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       BD   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IT   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       R(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Z�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  b   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       j�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       s�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181005190507  20181005190507  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               	A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @ף �ѻC1   @ףF)�r@3KƧ�c����m1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      	A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A���B   B  B  B  B   B(  B/��B7��B@  BH  BP  BW��B`  BhffBp  Bw��B��B�  B�  B�  B�  B�  B�  B�  B�  B���C  C  C  C�fC
  C  C  C  C  C  C  C  C�C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C��C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C��C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3D � DfD� D  D�fDfD� D  D�fDfD�fDfD�fDfD� D  D� D	  D	� D
  D
� D  D� DfD�fDfD� D��Dy�D  D� D  D� DfD� D��D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D   D � D ��D!y�D"  D"y�D#  D#� D$  D$� D%  D%�fD&fD&�fD'  D'� D(  D(�fD)  D)� D*  D*� D+fD+� D,  D,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2y�D2��D3y�D4  D4� D5  D5� D6  D6� D7  D7� D7��D8y�D8��D9y�D:  D:�fD;fD;�fD<  D<y�D<��D=� D>  DJ  DJ� DK  DK� DL  DLy�DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ�fDR  DR� DS  DS� DS��DTy�DU  DU�fDVfDV� DV��DW� DX  DX�fDY  DY� DZfDZ� D[  D[� D\  D\� D]  D]y�D]��D^� D_  D_y�D`  D`� Da  Day�Da��Db� DcfDc� Dd  Dd� DefDe�fDf  Df�fDgfDg�fDhfDh�fDh��Diy�Dj  Dj� Dk  Dky�Dk��Dly�Dl��Dmy�Dm��Dn� Do  Doy�Dp  Dp� Dq  Dq� DrfDr� Dr��Ds�fDt  Dt� DufDu� Dv  Dv� Dw  Dw� Dw�fDy�
D�!�D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��A ��A ��A@��A`��A�z�A�z�A�z�A�z�A�z�AϮA�z�A�G�B =qB=qB=qB=qB =qB(=qB/�B7�B@=qBH=qBP=qBW�B`=qBh��Bp=qBw�B�B��B��B��B��B��B��B��B��B��C\C\C\C��C
\C\C\C\C\C\C\C\C(�C\C\C \C"(�C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C8\C:\C<\C>\C@\CB\CD\CF\CH\CJ\CL\CN\CP\CR\CT\CV\CX\CZ\C\\C^\C`\Cb(�Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C�{C�{C�{C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C���C��C��C��C��C�{C��C��C��C��C�{C��C��C�{C��C��C��C��C��C��C��C��C��C��C�{C�{C��C�{C��C��C��C��C��C��C��C��C�{C��C�{C��C��C��C��C��C��C���C���C��C��C���C���C���C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C�{C�{C��C��C��C��C�{C��C��C��C��C��C��C���C���C���C���C�{C�{C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C���C��C��C���D ��D
=D��D�D�=D
=D��D�D�=D
=D�=D
=D�=D
=D��D�D��D	�D	��D
�D
��D�D��D
=D�=D
=D��D�qD}qD�D��D�D��D
=D��D�qD��D�D��D�D��D�D��D�D��D�D�=D�D��D�D��D�D��D
=D��D�D��D�D��D�D��D�D��D �D ��D �qD!}qD"�D"}qD#�D#��D$�D$��D%�D%�=D&
=D&�=D'�D'��D(�D(�=D)�D)��D*�D*��D+
=D+��D,�D,�=D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2}qD2�qD3}qD4�D4��D5�D5��D6�D6��D7�D7��D7�qD8}qD8�qD9}qD:�D:�=D;
=D;�=D<�D<}qD<�qD=��D>�DJ�DJ��DK�DK��DL�DL}qDM�DM��DN�DN��DO�DO��DP�DP�=DQ�DQ�=DR�DR��DS�DS��DS�qDT}qDU�DU�=DV
=DV��DV�qDW��DX�DX�=DY�DY��DZ
=DZ��D[�D[��D\�D\��D]�D]}qD]�qD^��D_�D_}qD`�D`��Da�Da}qDa�qDb��Dc
=Dc��Dd�Dd��De
=De�=Df�Df�=Dg
=Dg�=Dh
=Dh�=Dh�qDi}qDj�Dj��Dk�Dk}qDk�qDl}qDl�qDm}qDm�qDn��Do�Do}qDp�Dp��Dq�Dq��Dr
=Dr��Dr�qDs�=Dt�Dt��Du
=Du��Dv�Dv��Dw�Dw��Dw�=Dy��D�#�D��411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A͟�A�VA˶FA˸RA�ƨA���A���A˾wA˴9AˮA˧�Aˣ�Aˡ�A˛�A˕�A˕�A˕�Aˇ+A�5?A��yAɏ\A� �A�5?A�JA�(�A�JAǗ�A�ffA�ZA�XA�I�A�VA�\)A�hsAǟ�AǺ^AǾwAǬA�r�A�r�AÃA¥�A�ĜA���A�A���A�XA�A�n�A�jA���A�hsA�bNA��HA�ĜA��PA��wA�JA�VA�Q�A�\)A��^A�"�A��RA���A�%A���A���A���A�^5A��#A��+A���A�A�;dA��FA�r�A��A���A��A�=qA���A��HA�ĜA�~�A��HA��A�\)A��RA�ZA�1'A��
A�t�A��A�`BA�x�A~��Ay|�Av�Au��Al�HAh��Ag��Af��Ae��Adz�Ab�/Aa�Aa\)A^=qAY��AWS�AT1ARZAPffAM��AI��AE��AC�^ABȴABz�ABbAAp�A@{A>�+A<�yA;`BA:�A:��A:��A9��A7��A6^5A4ZA1��A0��A0�9A0^5A/|�A/\)A/33A-/A)��A(�jA)A)�A(  A'/A'�A&9XA%��A#�wA"bNA!�A!x�A �A�PA�A��A�A�RA?}AbNA7LA�AĜA�;AQ�A/A�HAhsA
�A
 �A�uAbA�FA5?A"�A�RA~�A^5A(�A��A+AQ�A�A\)A Q�@���@��@��@��!@�t�@��y@���@�S�@�@�9X@�$�@��@�o@�@�Z@�|�@���@�^5@��@�X@��@�  @�33@�\@�9X@���@�J@ݙ�@݉7@�%@���@�$�@ݺ^@݉7@�O�@�O�@ݺ^@��@ڇ+@��/@�ƨ@��@�ff@�hs@�r�@׍P@׾w@�ƨ@���@�ƨ@ץ�@ԣ�@�o@ѡ�@�9X@Ο�@�O�@��m@�\)@ʗ�@�{@��@�p�@��@�E�@�bN@î@�o@°!@�-@���@�?}@��@���@��@���@��@���@�|�@�o@�5?@���@�x�@�Ĝ@�(�@��P@�ȴ@�~�@�^5@��@�p�@�?}@�7L@���@���@�  @�33@��y@��@�hs@���@�b@���@�
=@�~�@�@���@��h@�hs@�x�@�x�@�hs@��-@�@��@�1@���@��H@�"�@�Ĝ@��h@���@��-@��-@�@��-@���@���@�hs@��@���@�bN@� �@��@��@���@��P@�t�@�\)@�S�@�+@�o@���@�n�@�$�@��@��@��@��@��@�hs@��@��@��@���@��/@��9@��@��F@���@���@���@�@���@�1'@��
@��@�@��@��-@�p�@�/@�&�@�&�@��@���@��D@�I�@�1'@��@�o@��H@��!@��+@�n�@�E�@�-@��^@�x�@�X@�%@���@�(�@�K�@�ȴ@�@���@��-@�O�@�?}@�?}@��`@���@�bN@��u@��j@��D@�9X@��
@�dZ@��!@��@��7@�X@�?}@��@���@�j@�1'@��@�K�@�33@�"�@�
=@��H@��!@�V@��#@��#@��#@���@��7@�p�@�`B@�G�@�V@�Ĝ@���@�bN@��@��F@��@��@�n�@�=q@�$�@��@��^@���@�hs@�7L@�V@��@�bN@�Q�@��w@�t�@�dZ@�33@�@��@�l�@{�$@l7�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A͟�A�VA˶FA˸RA�ƨA���A���A˾wA˴9AˮA˧�Aˣ�Aˡ�A˛�A˕�A˕�A˕�Aˇ+A�5?A��yAɏ\A� �A�5?A�JA�(�A�JAǗ�A�ffA�ZA�XA�I�A�VA�\)A�hsAǟ�AǺ^AǾwAǬA�r�A�r�AÃA¥�A�ĜA���A�A���A�XA�A�n�A�jA���A�hsA�bNA��HA�ĜA��PA��wA�JA�VA�Q�A�\)A��^A�"�A��RA���A�%A���A���A���A�^5A��#A��+A���A�A�;dA��FA�r�A��A���A��A�=qA���A��HA�ĜA�~�A��HA��A�\)A��RA�ZA�1'A��
A�t�A��A�`BA�x�A~��Ay|�Av�Au��Al�HAh��Ag��Af��Ae��Adz�Ab�/Aa�Aa\)A^=qAY��AWS�AT1ARZAPffAM��AI��AE��AC�^ABȴABz�ABbAAp�A@{A>�+A<�yA;`BA:�A:��A:��A9��A7��A6^5A4ZA1��A0��A0�9A0^5A/|�A/\)A/33A-/A)��A(�jA)A)�A(  A'/A'�A&9XA%��A#�wA"bNA!�A!x�A �A�PA�A��A�A�RA?}AbNA7LA�AĜA�;AQ�A/A�HAhsA
�A
 �A�uAbA�FA5?A"�A�RA~�A^5A(�A��A+AQ�A�A\)A Q�@���@��@��@��!@�t�@��y@���@�S�@�@�9X@�$�@��@�o@�@�Z@�|�@���@�^5@��@�X@��@�  @�33@�\@�9X@���@�J@ݙ�@݉7@�%@���@�$�@ݺ^@݉7@�O�@�O�@ݺ^@��@ڇ+@��/@�ƨ@��@�ff@�hs@�r�@׍P@׾w@�ƨ@���@�ƨ@ץ�@ԣ�@�o@ѡ�@�9X@Ο�@�O�@��m@�\)@ʗ�@�{@��@�p�@��@�E�@�bN@î@�o@°!@�-@���@�?}@��@���@��@���@��@���@�|�@�o@�5?@���@�x�@�Ĝ@�(�@��P@�ȴ@�~�@�^5@��@�p�@�?}@�7L@���@���@�  @�33@��y@��@�hs@���@�b@���@�
=@�~�@�@���@��h@�hs@�x�@�x�@�hs@��-@�@��@�1@���@��H@�"�@�Ĝ@��h@���@��-@��-@�@��-@���@���@�hs@��@���@�bN@� �@��@��@���@��P@�t�@�\)@�S�@�+@�o@���@�n�@�$�@��@��@��@��@��@�hs@��@��@��@���@��/@��9@��@��F@���@���@���@�@���@�1'@��
@��@�@��@��-@�p�@�/@�&�@�&�@��@���@��D@�I�@�1'@��@�o@��H@��!@��+@�n�@�E�@�-@��^@�x�@�X@�%@���@�(�@�K�@�ȴ@�@���@��-@�O�@�?}@�?}@��`@���@�bN@��u@��j@��D@�9X@��
@�dZ@��!@��@��7@�X@�?}@��@���@�j@�1'@��@�K�@�33@�"�@�
=@��H@��!@�V@��#@��#@��#@���@��7@�p�@�`B@�G�@�V@�Ĝ@���@�bN@��@��F@��@��@�n�@�=q@�$�@��@��^@���@�hs@�7L@�V@��@�bN@�Q�@��w@�t�@�dZ@�33@�@��@�l�@{�$@l7�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
��BBB%B+B1B1B	7B	7B	7B	7B	7B	7B	7B	7B
=B	7BB
��BBB
=B�B�B�BhB�B�B$�B+B8RB=qBA�BL�BS�B[#B_;BYBZB��BB�^B�fB��B+B�B(�B2-B9XB<jB@�B`BB{�B�B�=B�DB�B{�Br�Bt�Bv�Bw�By�By�Bx�B|�B|�Bz�Bu�BjBXBI�B8RB-B�BPB��B�sB�/B��B��B��B�Be`B=qB+B
��B
�B
��B
�=B
t�B
iyB
e`B
[#B
A�B
.B
\B	��B	�B	�B	��B	�VB	�7B	�B	y�B	n�B	hsB	`BB	K�B	1'B	'�B	%�B	�B	{B	+B�B�HB�B�B�B�B�B�B��B��B��B��B��B��B��BɺBǮBȴB��B��B��B��B��B��B��B�
B��B�
B�)B�fB�B�B�B�NB�/B�B��B��B��B�B�
B��B��B��B��B��B��B��B��B��B��BÖB�jB�!B��B��B��B��B�{B�oB�bB�\B�VB�VB�PB�JB�DB�=B�JB�VB�VB�VB�bB�hB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�?B�?B�?B�FB�FB�FB�LB�qB��B��B��B��B��B�/B�B�HB�#B�#B�B�B�B�B��B	B	B	B	B	B	B��B��B��B��B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	B	%B		7B	
=B	
=B	
=B	DB	PB	VB	\B	oB	�B	�B	�B	�B	�B	"�B	%�B	0!B	33B	7LB	<jB	A�B	A�B	E�B	J�B	N�B	P�B	S�B	W
B	\)B	^5B	_;B	_;B	_;B	bNB	dZB	e`B	l�B	r�B	y�B	� B	�B	�+B	�\B	��B	��B	��B	�B	�B	�!B	�-B	�-B	�3B	�9B	�LB	�^B	�jB	�wB	�}B	��B	��B	B	ÖB	B	B	ÖB	ĜB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ȴB	ǮB	ƨB	ƨB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
+B
+B
+B
1B
1B
	7B
	7B

=B
DB
JB
PB
PB
VB
\B
\B
VB
\B
bB
bB
bB
bB
oB
oB
oB
uB
uB
oB
oB
oB
uB
�B
!�B
-�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B
�B
��BBB%B+B1B1B	7B	7B	7B	7B	7B	7B	7B	7B
=B	7BB
��BBB
=B�B�B�BhB�B�B$�B+B8RB=qBA�BL�BS�B[#B_;BYBZB��BB�^B�fB��B+B�B(�B2-B9XB<jB@�B`BB{�B�B�=B�DB�B{�Br�Bt�Bv�Bw�By�By�Bx�B|�B|�Bz�Bu�BjBXBI�B8RB-B�BPB��B�sB�/B��B��B��B�Be`B=qB+B
��B
�B
��B
�=B
t�B
iyB
e`B
[#B
A�B
.B
\B	��B	�B	�B	��B	�VB	�7B	�B	y�B	n�B	hsB	`BB	K�B	1'B	'�B	%�B	�B	{B	+B�B�HB�B�B�B�B�B�B��B��B��B��B��B��B��BɺBǮBȴB��B��B��B��B��B��B��B�
B��B�
B�)B�fB�B�B�B�NB�/B�B��B��B��B�B�
B��B��B��B��B��B��B��B��B��B��BÖB�jB�!B��B��B��B��B�{B�oB�bB�\B�VB�VB�PB�JB�DB�=B�JB�VB�VB�VB�bB�hB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�?B�?B�?B�FB�FB�FB�LB�qB��B��B��B��B��B�/B�B�HB�#B�#B�B�B�B�B��B	B	B	B	B	B	B��B��B��B��B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	B	%B		7B	
=B	
=B	
=B	DB	PB	VB	\B	oB	�B	�B	�B	�B	�B	"�B	%�B	0!B	33B	7LB	<jB	A�B	A�B	E�B	J�B	N�B	P�B	S�B	W
B	\)B	^5B	_;B	_;B	_;B	bNB	dZB	e`B	l�B	r�B	y�B	� B	�B	�+B	�\B	��B	��B	��B	�B	�B	�!B	�-B	�-B	�3B	�9B	�LB	�^B	�jB	�wB	�}B	��B	��B	B	ÖB	B	B	ÖB	ĜB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ȴB	ǮB	ƨB	ƨB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
+B
+B
+B
1B
1B
	7B
	7B

=B
DB
JB
PB
PB
VB
\B
\B
VB
\B
bB
bB
bB
bB
oB
oB
oB
uB
uB
oB
oB
oB
uB
�B
!�B
-�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.06 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190507                              AO  ARCAADJP                                                                    20181005190507    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190507  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190507  QCF$                G�O�G�O�G�O�8000            