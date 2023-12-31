CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:34Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191734  20181005191734  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$v���1   @��%��@69������d��n��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B���B�  B�  B���B���C  C�C�C  C
  C�C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6�C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj�Cl  Cn  Cp  Cq�fCt  Cv�Cx  Cy�fC{�fC}�fC�fC��3C�  C�  C��3C��3C��3C��3C��3C�  C�  C��C��C�  C��C��C�  C�  C�  C��3C��3C��3C��C�  C�  C�  C��fC��3C��C�  C��3C�  C�  C�  C��C�  C��C��C��C��C�  C��3C��3C��C�  C�  C��C�  C�  C�  C��3C�  C��C��C��C�  C�  C�  C�  C��3C�  C��C��C��C��C��C�  C��3C��C�  C��C�  C�  C�  C�  C��3C�  C��C��C�  C�  C��3C��C��C�  C�  C��3C��C��C��C��C�  C��C��3C��3C��C��C�  C�  C��3C�  C��C��C�  C�  C��3C��3C��3C��3C�  C��C�  C��C��C��C��C�  C��3C�  C��C�  C�  C��C��C��C�  C��3C��3C��3D y�D  Dy�D��D� D  D� D��Dy�D  D� D  D�fD  Dy�D  D� D	  D	� D
fD
� D  D� D  D� D  D� D  D�fD  D� D��D� DfD� D��Dy�D  D�fD  D� D  D� DfD��DfD� D  Dy�D��Dy�D  D�fD  Dy�D��D�fD  D� D��D� D  D� D   D � D!  D!� D!��D"�fD#  D#y�D#��D$y�D%  D%y�D%��D&y�D&��D'� D(fD(� D)  D)� D*  D*y�D*��D+�fD,  D,y�D,��D-y�D.  D.�fD/  D/� D0  D0� D1fD1�fD2  D2y�D3  D3� D4  D4� D4��D5y�D6  D6�fD6��D7y�D7��D8y�D8��D9� D:fD:�fD;fD;�fD<fD<� D<�3D=y�D=��D>� D?  D?�fD@  D@� DA  DA� DB  DB� DC  DC�fDC��DD�fDEfDE�fDF  DFy�DF��DG� DHfDH� DIfDI� DJ  DJ� DK  DKy�DK��DLy�DL��DMs3DM�3DN� DO  DOy�DO��DPy�DP��DQ� DRfDR� DS  DS� DT  DT�fDUfDU� DV  DV� DV��DW�fDX�DX�fDYfDY�fDZfDZ� D[  D[� D\fD\�fD]fD]� D]��D^y�D^��D_y�D_��D`� Da  Da� DbfDb��DcfDc� Dd  Dd� Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dj��Dk� DlfDl�fDmfDm�fDnfDny�Dn��Doy�Dp  Dp�fDqfDq�fDrfDr� Ds  Dsy�Dt  Dty�Du  Du� Du��Dvy�Dw  Dwy�DwٚDy��D�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@ȣ�AQ�A$Q�ADQ�AdQ�A�(�A�(�A�(�A�(�A���A�(�A�(�A�(�B{B	{B{B{B!{B){B1{B9{BA{BI{BQ{BY{Baz�Bi{Bq{By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=B�W
B܊=B��=B�=B�=B�=B�W
B�=B��=B�W
C +�CEC^�C^�CEC
EC^�CECEC^�CECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4^�C6^�C8EC:EC<EC>EC@ECBECDECF+�CHECJECLECNECPECRECTECVECXECZEC\EC^EC`ECbECdECfECh^�Cj^�ClECnECpECr+�CtECv^�CxECz+�C|+�C~+�C��C��C�"�C�"�C��C��C��C��C��C�"�C�"�C�/\C�/\C�"�C�/\C�/\C�"�C�"�C�"�C��C��C��C�/\C�"�C�"�C�"�C��C��C�/\C�"�C��C�"�C�"�C�"�C�/\C�"�C�/\C�/\C�/\C�/\C�"�C��C��C�/\C�"�C�"�C�/\C�"�C�"�C�"�C��C�"�C�/\C�/\C�/\C�"�C�"�C�"�C�"�C��C�"�C�/\C�/\C�/\C�/\C�/\C�"�C��C�/\C�"�C�/\C�"�C�"�C�"�C�"�C��C�"�C�/\C�/\C�"�C�"�C��C�/\C�/\C�"�C�"�C��C�/\C�/\C�/\C�/\C�"�C�<)C��C��C�/\C�/\C�"�C�"�C��C�"�C�/\C�/\C�"�C�"�C��C��C��C��C�"�C�/\C�"�C�/\C�/\C�/\C�/\C�"�C��C�"�C�/\C�"�C�"�C�/\C�/\C�/\C�"�C��C��D 
�D ��DHD��D
�D�HDHD�HD
�D��DHD�HDHD��DHD��DHD�HD	HD	�HD
�D
�HDHD�HDHD�HDHD�HDHD��DHD�HD
�D�HD�D�HD
�D��DHD��DHD�HDHD�HD�D�D�D�HDHD��D
�D��DHD��DHD��D
�D��DHD�HD
�D�HDHD�HD HD �HD!HD!�HD"
�D"��D#HD#��D$
�D$��D%HD%��D&
�D&��D'
�D'�HD(�D(�HD)HD)�HD*HD*��D+
�D+��D,HD,��D-
�D-��D.HD.��D/HD/�HD0HD0�HD1�D1��D2HD2��D3HD3�HD4HD4�HD5
�D5��D6HD6��D7
�D7��D8
�D8��D9
�D9�HD:�D:��D;�D;��D<�D<�HD={D=��D>
�D>�HD?HD?��D@HD@�HDAHDA�HDBHDB�HDCHDC��DD
�DD��DE�DE��DFHDF��DG
�DG�HDH�DH�HDI�DI�HDJHDJ�HDKHDK��DL
�DL��DM
�DM�{DN{DN�HDOHDO��DP
�DP��DQ
�DQ�HDR�DR�HDSHDS�HDTHDT��DU�DU�HDVHDV�HDW
�DW��DXDX��DY�DY��DZ�DZ�HD[HD[�HD\�D\��D]�D]�HD^
�D^��D_
�D_��D`
�D`�HDaHDa�HDb�Db�Dc�Dc�HDdHDd�HDe
�De�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDk
�Dk�HDl�Dl��Dm�Dm��Dn�Dn��Do
�Do��DpHDp��Dq�Dq��Dr�Dr�HDsHDs��DtHDt��DuHDu�HDv
�Dv��DwHDw��Dw��Dy��D�!�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�$�A�-A�1'A�5?A�5?A�33A�33A�7LA�33A�33A�5?A�5?A�7LA�7LA�9XA�9XA�9XA�9XA�;dA�=qA�=qA�=qA�=qA�?}A�=qA�33A�1'A�/A�+A�(�A�%A̴9A�XA�33A�-A��A���A˓uA�-A���Aʏ\A��A�jA� �A��Aȗ�AǋDA�(�A�JA��;A���A�ƨAğ�A���A�&�A�A��+A�I�A�
=A��A��A�E�A�ĜA��A��A�9XA���A�33A�ZA���A�\)A�ffA�-A���A���A�n�A��A�~�A�bA���A�9XA�ĜA��hA��A���A��HA�l�A�VA��A�  A�JA��mA��A�A��
A�|�A�$�A��FA�(�A���A��A�dZA�5?A�A�A���A�G�A��mA��^A��A��A���A�jA�ĜA�|�A��
A���A� �A�jA���A��9A�ffA��;A�Q�A~��A}�A|v�A{�TAz{Awl�AvAtr�AtAsx�Ar^5AqG�Ao�hAmK�Ak
=AiO�AfjAd��Ac�Aa��A`{A^�RA]�hA[�FAXM�AT��AS�ARv�AQ�;AQ�AQVAP�AOp�AN�\AM�AK��AJ��AI�^AH�AF��AE��AE;dAD�+AD-AC�-ABbA?G�A=�A=t�A<��A;XA9�FA9hsA8v�A6�A5��A4-A3x�A3A1��A0��A01'A.�A-7LA,ffA+C�A*bA)+A(E�A'p�A'/A%\)A$  A#+A"�RA"A�A I�A��A~�A�A/AA��A`BA$�At�A�#A+A�#A1'AƨA�hAXA�A
=A	A�A=qA�A��AȴA1'A+AVA��A��A�
Al�A�A M�@��H@�$�@���@���@�z�@�I�@���@��
@�?}@��
@��`@�1'@�dZ@�V@��#@���@�
=@�!@��@߾w@�@�p�@ܴ9@�@�@��y@��@���@�33@��y@�^5@д9@�S�@��H@Ώ\@�=q@Ͳ-@̬@�9X@˕�@�v�@�/@��@ȓu@��
@��y@�E�@�/@��`@��`@�9X@�@�Z@��@��@�@���@���@�A�@�l�@��`@���@�bN@�Q�@���@�1@�bN@��w@��@�G�@��@���@�@�X@���@�E�@�x�@�V@��D@���@��#@��/@�r�@� �@��P@�o@���@�o@�"�@�o@��H@��!@��H@�ff@�=q@�{@��^@�O�@�A�@�9X@�1'@���@�;d@�o@��H@�v�@�;d@���@�S�@�
=@�ȴ@���@��^@���@�hs@��@��@�Z@�1'@�1@��
@�dZ@���@���@��+@�v�@��@��@�?}@�O�@�X@�`B@�X@��@��/@�r�@�bN@�A�@�9X@��P@�S�@�33@���@�-@��#@��h@��@�hs@�X@�?}@���@��@�C�@�v�@���@��7@�V@��/@���@��j@���@�j@� �@��;@��@���@�l�@�dZ@�\)@�33@�o@���@��@�n�@�-@��@���@���@��h@�G�@�&�@���@���@���@�A�@�l�@�\)@�l�@�l�@�K�@���@��+@�v�@�5?@�J@��@�%@�Ĝ@��D@�I�@�(�@��@���@�K�@�ȴ@���@�~�@��@��h@��@�?}@�&�@���@�(�@��@���@�S�@�K�@�33@�"�@��@���@�ff@��@���@��h@�X@��@���@��@���@�|�@�33@��H@��@�ȴ@��R@���@�n�@�-@��@�@��^@��7@�X@�&�@���@�z�@�Z@�Q�@�I�@�  @��m@��
@���@�ƨ@���@��@�+@��H@��!@��\@�%�@|�|11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�$�A�-A�1'A�5?A�5?A�33A�33A�7LA�33A�33A�5?A�5?A�7LA�7LA�9XA�9XA�9XA�9XA�;dA�=qA�=qA�=qA�=qA�?}A�=qA�33A�1'A�/A�+A�(�A�%A̴9A�XA�33A�-A��A���A˓uA�-A���Aʏ\A��A�jA� �A��Aȗ�AǋDA�(�A�JA��;A���A�ƨAğ�A���A�&�A�A��+A�I�A�
=A��A��A�E�A�ĜA��A��A�9XA���A�33A�ZA���A�\)A�ffA�-A���A���A�n�A��A�~�A�bA���A�9XA�ĜA��hA��A���A��HA�l�A�VA��A�  A�JA��mA��A�A��
A�|�A�$�A��FA�(�A���A��A�dZA�5?A�A�A���A�G�A��mA��^A��A��A���A�jA�ĜA�|�A��
A���A� �A�jA���A��9A�ffA��;A�Q�A~��A}�A|v�A{�TAz{Awl�AvAtr�AtAsx�Ar^5AqG�Ao�hAmK�Ak
=AiO�AfjAd��Ac�Aa��A`{A^�RA]�hA[�FAXM�AT��AS�ARv�AQ�;AQ�AQVAP�AOp�AN�\AM�AK��AJ��AI�^AH�AF��AE��AE;dAD�+AD-AC�-ABbA?G�A=�A=t�A<��A;XA9�FA9hsA8v�A6�A5��A4-A3x�A3A1��A0��A01'A.�A-7LA,ffA+C�A*bA)+A(E�A'p�A'/A%\)A$  A#+A"�RA"A�A I�A��A~�A�A/AA��A`BA$�At�A�#A+A�#A1'AƨA�hAXA�A
=A	A�A=qA�A��AȴA1'A+AVA��A��A�
Al�A�A M�@��H@�$�@���@���@�z�@�I�@���@��
@�?}@��
@��`@�1'@�dZ@�V@��#@���@�
=@�!@��@߾w@�@�p�@ܴ9@�@�@��y@��@���@�33@��y@�^5@д9@�S�@��H@Ώ\@�=q@Ͳ-@̬@�9X@˕�@�v�@�/@��@ȓu@��
@��y@�E�@�/@��`@��`@�9X@�@�Z@��@��@�@���@���@�A�@�l�@��`@���@�bN@�Q�@���@�1@�bN@��w@��@�G�@��@���@�@�X@���@�E�@�x�@�V@��D@���@��#@��/@�r�@� �@��P@�o@���@�o@�"�@�o@��H@��!@��H@�ff@�=q@�{@��^@�O�@�A�@�9X@�1'@���@�;d@�o@��H@�v�@�;d@���@�S�@�
=@�ȴ@���@��^@���@�hs@��@��@�Z@�1'@�1@��
@�dZ@���@���@��+@�v�@��@��@�?}@�O�@�X@�`B@�X@��@��/@�r�@�bN@�A�@�9X@��P@�S�@�33@���@�-@��#@��h@��@�hs@�X@�?}@���@��@�C�@�v�@���@��7@�V@��/@���@��j@���@�j@� �@��;@��@���@�l�@�dZ@�\)@�33@�o@���@��@�n�@�-@��@���@���@��h@�G�@�&�@���@���@���@�A�@�l�@�\)@�l�@�l�@�K�@���@��+@�v�@�5?@�J@��@�%@�Ĝ@��D@�I�@�(�@��@���@�K�@�ȴ@���@�~�@��@��h@��@�?}@�&�@���@�(�@��@���@�S�@�K�@�33@�"�@��@���@�ff@��@���@��h@�X@��@���@��@���@�|�@�33@��H@��@�ȴ@��R@���@�n�@�-@��@�@��^@��7@�X@�&�@���@�z�@�Z@�Q�@�I�@�  @��m@��
@���@�ƨ@���@��@�+@��H@��!@��\@�%�@|�|11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BBBBBBBBBBBBBBBBBBBBBBBBBB%B+B
=BJBPBhB�B�B �B)�B7LB:^B?}BF�BO�BVB\)B^5B\)BZB[#BbNBcTBl�Bu�Bu�Bu�Bu�B�B�B�B�B�=B�bB��B��B��B��B��B�B�'B�'B�B��B��B��B��B��B��B�B�B�9B�B��B��B�uB�Bt�Bn�Be`B]/BYBT�BP�BK�BF�B)�B"�B�BuBhBhB\BoB�BPB	7BB��B��B�B�B�wB��B�Bt�BjB<jB �BbB
�B
�/B
ÖB
�B
��B
��B
�hB
�7B
}�B
v�B
n�B
iyB
]/B
K�B
B�B
7LB
33B
/B
'�B
�B
hB
B	�B	�`B	��B	ƨB	�^B	�-B	��B	��B	��B	�7B	t�B	aHB	XB	T�B	P�B	N�B	K�B	E�B	A�B	<jB	6FB	/B	(�B	#�B	�B	�B	hB	PB		7B	+B	B��B�B�B�B�B�mB�TB�NB�BB�)B�
B��B��B��BɺBŢBB�wB�XB�?B�'B�B�B��B��B��B��B��B��B��B�{B�bB�DB�+B�B}�By�Bw�Bt�Bq�Bo�Bl�BiyBgmBdZBcTBbNB`BB]/BYBVBT�BR�BR�BQ�BP�BP�BO�BO�BN�BM�BM�BN�BS�BXBVBS�BS�BQ�BR�BZB[#BP�BI�BF�BF�BE�BD�BJ�BP�BQ�BQ�BQ�BP�BL�BI�BG�BG�BF�BF�BI�BI�BH�BH�BH�BK�BN�BP�BQ�BS�BVB^5Be`BgmBhsBhsBgmBhsBgmBdZBffBffBhsBjBk�BjBiyBt�Bw�Bx�B}�B�\B�bB�hB�oB��B�{B�{B��B��B�hB��B��B��B��B��B��B�BĜB�
B�
B��B��B��B�
B��B�B�#B�/B�HB�`B�mB�B�B�B�B�B�B��B��B��B��B	B	+B	DB	\B	bB	oB	�B	�B	�B	'�B	1'B	49B	33B	49B	5?B	7LB	7LB	8RB	;dB	@�B	B�B	C�B	E�B	E�B	J�B	N�B	P�B	Q�B	R�B	XB	[#B	^5B	_;B	`BB	`BB	`BB	`BB	cTB	e`B	hsB	iyB	jB	jB	jB	k�B	m�B	s�B	u�B	v�B	x�B	z�B	|�B	� B	�B	�B	� B	�B	�B	�1B	�PB	�hB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�!B	�!B	�!B	�'B	�-B	�-B	�'B	�-B	�-B	�'B	�-B	�?B	�LB	�LB	�XB	�^B	�dB	�qB	�}B	�}B	��B	��B	B	ÖB	ĜB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�HB	�NB	�NB	�TB	�`B	�fB	�mB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B
622222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   BBBBBBBBBBBBBBBBBBBBBBBBBB%B+B
=BJBPBhB�B�B �B)�B7LB:^B?}BF�BO�BVB\)B^5B\)BZB[#BbNBcTBl�Bu�Bu�Bu�Bu�B�B�B�B�B�=B�bB��B��B��B��B��B�B�'B�'B�B��B��B��B��B��B��B�B�B�9B�B��B��B�uB�Bt�Bn�Be`B]/BYBT�BP�BK�BF�B)�B"�B�BuBhBhB\BoB�BPB	7BB��B��B�B�B�wB��B�Bt�BjB<jB �BbB
�B
�/B
ÖB
�B
��B
��B
�hB
�7B
}�B
v�B
n�B
iyB
]/B
K�B
B�B
7LB
33B
/B
'�B
�B
hB
B	�B	�`B	��B	ƨB	�^B	�-B	��B	��B	��B	�7B	t�B	aHB	XB	T�B	P�B	N�B	K�B	E�B	A�B	<jB	6FB	/B	(�B	#�B	�B	�B	hB	PB		7B	+B	B��B�B�B�B�B�mB�TB�NB�BB�)B�
B��B��B��BɺBŢBB�wB�XB�?B�'B�B�B��B��B��B��B��B��B��B�{B�bB�DB�+B�B}�By�Bw�Bt�Bq�Bo�Bl�BiyBgmBdZBcTBbNB`BB]/BYBVBT�BR�BR�BQ�BP�BP�BO�BO�BN�BM�BM�BN�BS�BXBVBS�BS�BQ�BR�BZB[#BP�BI�BF�BF�BE�BD�BJ�BP�BQ�BQ�BQ�BP�BL�BI�BG�BG�BF�BF�BI�BI�BH�BH�BH�BK�BN�BP�BQ�BS�BVB^5Be`BgmBhsBhsBgmBhsBgmBdZBffBffBhsBjBk�BjBiyBt�Bw�Bx�B}�B�\B�bB�hB�oB��B�{B�{B��B��B�hB��B��B��B��B��B��B�BĜB�
B�
B��B��B��B�
B��B�B�#B�/B�HB�`B�mB�B�B�B�B�B�B��B��B��B��B	B	+B	DB	\B	bB	oB	�B	�B	�B	'�B	1'B	49B	33B	49B	5?B	7LB	7LB	8RB	;dB	@�B	B�B	C�B	E�B	E�B	J�B	N�B	P�B	Q�B	R�B	XB	[#B	^5B	_;B	`BB	`BB	`BB	`BB	cTB	e`B	hsB	iyB	jB	jB	jB	k�B	m�B	s�B	u�B	v�B	x�B	z�B	|�B	� B	�B	�B	� B	�B	�B	�1B	�PB	�hB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�!B	�!B	�!B	�'B	�-B	�-B	�'B	�-B	�-B	�'B	�-B	�?B	�LB	�LB	�XB	�^B	�dB	�qB	�}B	�}B	��B	��B	B	ÖB	ĜB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�HB	�NB	�NB	�TB	�`B	�fB	�mB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B
622222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191734                              AO  ARCAADJP                                                                    20181005191734    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191734  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191734  QCF$                G�O�G�O�G�O�8000            