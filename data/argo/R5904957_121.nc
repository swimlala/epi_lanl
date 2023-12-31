CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:27Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140827  20181024140827  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               yA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @����%=
1   @���ffy@4h1&�x��c�I�^1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      yA   A   B   @y��@�  A   A   A@  A^ffA�  A���A�  A�33A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CQ�fCS�fCU�fCW�fCY�fC\  C^  C`  Cb  Cd  Cf�Ch�Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C��3C��3C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D�fD  D� D  D� D  D� DfD�fD	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��D� D��D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D�fD   D � D ��D!� D"  D"� D.� D/  D/y�D/��D0y�D0��D1y�D2  D2� D2��D3y�D3��D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJfDJ� DJ��DKy�DL  DL� DM  DMy�DM��DN� DO  DO�fDPfDP� DQ  DQ�fDRfDR� DS  DS� DT  DT� DU  DU� DV  DV�fDWfDW� DW��DXy�DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_�fD`  D`y�Da  Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� Df  Dfy�Dg  Dg� Dh  Dh� Di  Diy�Di��Dj� DkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Dn��Doy�Dp  Dp� DqfDq� Dr  Dr� Ds  Ds�fDtfDt� Du  Du� Dv  Dv� Dw  Dwy�Dy�RD�EqD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @}p�@��A ��A ��A@��A_\)A�z�A�G�A�z�A��A�z�A�z�A�G�A�z�B =qB=qB=qB=qB =qB(=qB0=qB8=qB?�BH=qBP=qBX=qB`=qBh=qBp=qBx=qB��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C \C\C\C\C\C
\C\C\C\C(�C\C\C\C(�C\C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C8\C:\C<\C>\C@\CB\CD\CF\CH\CJ\CL\CN\CP\CQ��CS��CU��CW��CY��C\\C^\C`\Cb\Cd\Cf(�Ch(�Cj(�Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C�{C���C���C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C���C��C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C���C��C�{C��C���C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C���C��C�{C�{C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D�=D�D��D�D��D�D��D
=D�=D	�D	��D
�D
�=D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D}qD�qD��D�qD��D�D��D�qD��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�qD��D�D�=D �D ��D �qD!��D"�D"��D.��D/�D/}qD/�qD0}qD0�qD1}qD2�D2��D2�qD3}qD3�qD4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=
=D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ
=DJ��DJ�qDK}qDL�DL��DM�DM}qDM�qDN��DO�DO�=DP
=DP��DQ�DQ�=DR
=DR��DS�DS��DT�DT��DU�DU��DV�DV�=DW
=DW��DW�qDX}qDY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_�=D`�D`}qDa�Da��Db�Db��Dc�Dc��Dd�Dd��De
=De��Df�Df}qDg�Dg��Dh�Dh��Di�Di}qDi�qDj��Dk
=Dk��Dl�Dl��Dm�Dm��Dn�Dn��Dn�qDo}qDp�Dp��Dq
=Dq��Dr�Dr��Ds�Ds�=Dt
=Dt��Du�Du��Dv�Dv��Dw�Dw}qDy�)D�G]D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��mA��mA��mA��A��yA��yA��A��A��A��A��A��yA��yA��A��A���A���A���A�ȴA�jA���A۸RA�5?A��A��yA�`BA���A�(�A�A��A��Aɝ�A��HA���A��A��
A�Q�A��TA��`A�G�A� �A�VA��A�?}A��A��^A�ZA��A��A���A���A��A�  A��A��A��#A��#A�ffA�^5A�E�A�JA���A��\A�l�A��A��yA�/A��A��DA�C�A�7LA�{A���A���A�M�A�x�A��9A�A���A��A�r�A���A���A�p�A��9A�$�A��A�bNA��-A�ĜA�E�A��#A���A���A���A�jA�l�A~�jA{+AzVAw��At�!Aq�Aq%ApA�Am&�Ai�hAh�Aa�A`1'A_/A^�DA[�hAW/ATn�AQ�;APȴAO�7AOoAN�`ANQ�AL�AH�9AF1'AEVAB�/A@�A@��A@z�A@$�A?7LA>Q�A=��A<�\A:VA69XA2�RA0�`A0$�A.�/A.1'A-�#A,�A,z�A, �A+C�A)�PA(�jA'�A&�`A&��A%�FA%"�A$��A$r�A#;dA"A!�;A!�PA �A I�A|�A~�A=qAVA�AA�AC�A��A�mA�/A�/An�A��A?}A�!A�A��A�hA��AZA/A
�uA	��A=qA�A��A{A�^A|�A�HA��A��A��A��A-Al�A n�A  �@�S�@�p�@��H@��@��#@��/@��j@�I�@�|�@�M�@���@�w@�+@�?}@��@���@�33@� �@�M�@�G�@��
@�7@�Q�@�M�@݉7@��@���@�r�@��@ڰ!@٩�@��`@؋D@�r�@�j@�Z@�9X@���@ם�@�dZ@���@�$�@�j@�;d@�@�$�@�V@� �@�ƨ@υ@���@�ff@�$�@Ͳ-@���@�1@��
@�ƨ@˥�@�S�@�@���@�A�@�9X@�%@��@��@�@Ə\@Ĭ@�S�@�J@��@��@��
@�+@��R@�5?@���@�bN@��w@�C�@��H@���@�M�@�G�@��/@���@���@���@�Ĝ@��`@��j@��D@�z�@�j@�Z@�A�@�1@���@��@��@��@��@��@���@�V@���@��@�Ĝ@�(�@�\)@��y@��+@���@�?}@��@��u@�r�@�Z@�(�@�t�@���@��\@��@��@��T@�/@�K�@�^5@�@��@�E�@�hs@�7L@�G�@��`@�"�@��^@�n�@�o@��H@��@��@��R@���@�@���@��@��@�ff@�ȴ@�
=@�V@�J@���@��@��@��7@���@���@�X@�&�@��@� �@�  @���@���@���@���@��@��@���@��@�o@�ff@���@�j@��u@���@��@�9X@�b@� �@��/@�%@��@��D@�bN@�A�@�1@�t�@�"�@�
=@���@�@�@��y@��!@�~�@���@�hs@�&�@���@��@���@�r�@�bN@�Q�@�A�@� �@�b@��m@�o@�=q@��@��@�@��^@��h@�`B@�O�@�G�@�7L@��`@���@�j@�bN@�I�@�(�@�b@�1@��
@�S�@�@��@���@�v�@�ff@�V@�V@�V@�V@�J@��#@���@�hs@�G�@��@�V@�%@���@��@���@��u@�I�@�  @���@��;@���@�l�@�C�@�33@�33@�+@��@�ȴ@��R@���@�~�@�n�@���@~��@i�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A��mA��mA��mA��A��yA��yA��A��A��A��A��A��yA��yA��A��A���A���A���A�ȴA�jA���A۸RA�5?A��A��yA�`BA���A�(�A�A��A��Aɝ�A��HA���A��A��
A�Q�A��TA��`A�G�A� �A�VA��A�?}A��A��^A�ZA��A��A���A���A��A�  A��A��A��#A��#A�ffA�^5A�E�A�JA���A��\A�l�A��A��yA�/A��A��DA�C�A�7LA�{A���A���A�M�A�x�A��9A�A���A��A�r�A���A���A�p�A��9A�$�A��A�bNA��-A�ĜA�E�A��#A���A���A���A�jA�l�A~�jA{+AzVAw��At�!Aq�Aq%ApA�Am&�Ai�hAh�Aa�A`1'A_/A^�DA[�hAW/ATn�AQ�;APȴAO�7AOoAN�`ANQ�AL�AH�9AF1'AEVAB�/A@�A@��A@z�A@$�A?7LA>Q�A=��A<�\A:VA69XA2�RA0�`A0$�A.�/A.1'A-�#A,�A,z�A, �A+C�A)�PA(�jA'�A&�`A&��A%�FA%"�A$��A$r�A#;dA"A!�;A!�PA �A I�A|�A~�A=qAVA�AA�AC�A��A�mA�/A�/An�A��A?}A�!A�A��A�hA��AZA/A
�uA	��A=qA�A��A{A�^A|�A�HA��A��A��A��A-Al�A n�A  �@�S�@�p�@��H@��@��#@��/@��j@�I�@�|�@�M�@���@�w@�+@�?}@��@���@�33@� �@�M�@�G�@��
@�7@�Q�@�M�@݉7@��@���@�r�@��@ڰ!@٩�@��`@؋D@�r�@�j@�Z@�9X@���@ם�@�dZ@���@�$�@�j@�;d@�@�$�@�V@� �@�ƨ@υ@���@�ff@�$�@Ͳ-@���@�1@��
@�ƨ@˥�@�S�@�@���@�A�@�9X@�%@��@��@�@Ə\@Ĭ@�S�@�J@��@��@��
@�+@��R@�5?@���@�bN@��w@�C�@��H@���@�M�@�G�@��/@���@���@���@�Ĝ@��`@��j@��D@�z�@�j@�Z@�A�@�1@���@��@��@��@��@��@���@�V@���@��@�Ĝ@�(�@�\)@��y@��+@���@�?}@��@��u@�r�@�Z@�(�@�t�@���@��\@��@��@��T@�/@�K�@�^5@�@��@�E�@�hs@�7L@�G�@��`@�"�@��^@�n�@�o@��H@��@��@��R@���@�@���@��@��@�ff@�ȴ@�
=@�V@�J@���@��@��@��7@���@���@�X@�&�@��@� �@�  @���@���@���@���@��@��@���@��@�o@�ff@���@�j@��u@���@��@�9X@�b@� �@��/@�%@��@��D@�bN@�A�@�1@�t�@�"�@�
=@���@�@�@��y@��!@�~�@���@�hs@�&�@���@��@���@�r�@�bN@�Q�@�A�@� �@�b@��m@�o@�=q@��@��@�@��^@��h@�`B@�O�@�G�@�7L@��`@���@�j@�bN@�I�@�(�@�b@�1@��
@�S�@�@��@���@�v�@�ff@�V@�V@�V@�V@�J@��#@���@�hs@�G�@��@�V@�%@���@��@���@��u@�I�@�  @���@��;@���@�l�@�C�@�33@�33@�+@��@�ȴ@��R@���@�~�@�n�@���@~��@i�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�=B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�=B�7B�7B�7B�7B�=B�7B�JB��B��B�LB�RB�LB�LBǮB�5B�sB��B  B��B��B%B%BDB�B6FBH�BS�BXBT�BS�B[#B^5BZBYBP�BL�BI�BG�BD�B@�B7LB+B%�B�B�B�BoB
=BB��B�`B�B�B��B��B��BȴBB�3B��B��B�DB�B�B�B�Bn�BN�B9XB1'B2-B$�B{B\B%�B
��B
�NB
��B
�hB
��B
x�B
\)B
H�B
0!B
&�B
uB	��B	�mB	�;B	�B	�LB	�\B	{�B	P�B	R�B	Q�B	N�B	F�B	+B	�B	VB	JB	
=B	+B	B	  B��B�B�HB�#B�B��B��B��B��B��BǮBŢB��B�jB�9B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�hB�bB�\B�VB�JB�JB�7B�B�B�B�VB�hB�DB�1B�1B�%B�=B�JB��B�uB�uB�bB�VB�VB�\B�uB��B��B��B�B�3B�wBBÖBBÖB��BB��B�}B�qB�qB�jB�^B�dB�XB�jB�^B�LB�-B�!B�B��B��B��B�B�}B��BǮBÖB��B�wB�qB�qB�qB�}BBƨBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�NB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B	JB	\B	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	�B	�B	�B	�B	�B	!�B	$�B	&�B	'�B	,B	33B	5?B	7LB	9XB	:^B	;dB	<jB	>wB	?}B	A�B	E�B	F�B	I�B	M�B	L�B	N�B	R�B	XB	YB	\)B	]/B	^5B	bNB	e`B	gmB	hsB	jB	l�B	p�B	r�B	u�B	v�B	w�B	{�B	|�B&�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�'B	�LB	�RB	�9B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�3B	�FB	�LB	�jB	�wB	�}B	��B	B	ŢB	ŢB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�
B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�)B	�5B	�;B	�BB	�NB	�ZB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B
1B
	7B

XB
�B
-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�=B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�=B�7B�7B�7B�7B�=B�7B�JB��B��B�LB�RB�LB�LBǮB�5B�sB��B  B��B��B%B%BDB�B6FBH�BS�BXBT�BS�B[#B^5BZBYBP�BL�BI�BG�BD�B@�B7LB+B%�B�B�B�BoB
=BB��B�`B�B�B��B��B��BȴBB�3B��B��B�DB�B�B�B�Bn�BN�B9XB1'B2-B$�B{B\B%�B
��B
�NB
��B
�hB
��B
x�B
\)B
H�B
0!B
&�B
uB	��B	�mB	�;B	�B	�LB	�\B	{�B	P�B	R�B	Q�B	N�B	F�B	+B	�B	VB	JB	
=B	+B	B	  B��B�B�HB�#B�B��B��B��B��B��BǮBŢB��B�jB�9B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�hB�bB�\B�VB�JB�JB�7B�B�B�B�VB�hB�DB�1B�1B�%B�=B�JB��B�uB�uB�bB�VB�VB�\B�uB��B��B��B�B�3B�wBBÖBBÖB��BB��B�}B�qB�qB�jB�^B�dB�XB�jB�^B�LB�-B�!B�B��B��B��B�B�}B��BǮBÖB��B�wB�qB�qB�qB�}BBƨBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�NB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B	JB	\B	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	�B	�B	�B	�B	�B	!�B	$�B	&�B	'�B	,B	33B	5?B	7LB	9XB	:^B	;dB	<jB	>wB	?}B	A�B	E�B	F�B	I�B	M�B	L�B	N�B	R�B	XB	YB	\)B	]/B	^5B	bNB	e`B	gmB	hsB	jB	l�B	p�B	r�B	u�B	v�B	w�B	{�B	|�B&�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�'B	�LB	�RB	�9B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�3B	�FB	�LB	�jB	�wB	�}B	��B	B	ŢB	ŢB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�
B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�)B	�5B	�;B	�BB	�NB	�ZB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B
1B
	7B

XB
�B
-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.06 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140827                              AO  ARCAADJP                                                                    20181024140827    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140827  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140827  QCF$                G�O�G�O�G�O�0               