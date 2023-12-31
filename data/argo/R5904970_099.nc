CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  	   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:15:18Z creation      
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
resolution        =���   axis      Z        $  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     $  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     $  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ^   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $  `$   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   hH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $  jT   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $  rx   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $  |�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �,   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �,   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �,   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �,   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024141518  20181024141518  5904970 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               cA   AO  6785                            2B  A   APEX                            7726                            111215                          846 @��䩸�1   @���33E�@3�&�x���cف$�/1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      cB   B   B   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���B   B  B  B  B   B(  B0  B8  B@ffBHffBPffBW��B`  Bh  Bp  Bx  B�  B�33B�  B���B���B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D y�D ��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	y�D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH�fDI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DVfDV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[fD[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw��Dy� D�G\D���11111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @;�@{�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�=qA�=qA�p�B�RB�RB�RB�RB'�RB/�RB7�RB@�BH�BP�BWQ�B_�RBg�RBo�RBw�RB�RB�\B��)B���B���B���B��)B���B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B���C�C�C�C�C	�C�C�C�C�C�C�B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B���C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C"�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��=C��
C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��=C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��C��
D uD �DuD��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	uD	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH��DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR��DR��DS{�DS��DT{�DT��DU{�DV�DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�D[�D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw�RDy��D�ED���11111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A߲-A߾wA�ĜA�ĜA�ƨA�ƨA�ĜAߴ9Aߡ�AߓuA�~�A�E�A�?}A�?}A�;dA�7LA�5?A�5?A�5?A�33A�33A�/A�+A� �Aޏ\A��A�|�AыDA�r�A�ffA�|�Aˏ\A�?}AǶFA�S�A��TA���A��;A�A�A���A�K�A��PA�;dA�%A��9A�5?A�VA�VA��+A��HA���A��A��A�{A�G�A�ĜA�ffA�C�A�+A��wA��!A�ĜA��!A�ĜA���A�A�ĜA��hA��jA� �A��`A��A��A�C�A�
=A�{A�G�A�ĜA�ffA�C�A�+A��wA��!A�ĜA��!A�ĜA���A�A�ĜA��hA��jA� �A��`A��A��A�C�A�
=A��TA�"�A�C�A��A���A���A�^5A��^A���A��PA��A��mA���A��A��#A�  A�ĜA�oA��uA���A��jA}�A{�PAzbAx�RAwoAvA�At�As/Aq��Aq�Ap�9Ao��Ao\)Al��Aj�AioAhv�Ag�PAfr�Ab�Aal�A_��A\�\AZJAW�AU�^ATffAS|�AR=qAP�\APE�AP{AO+AK�TAJ{AHI�AF�/AE
=AB�AB  A?�A>��A=��A<VA;oA9�;A9/A7C�A5��A5K�A5A4�\A3��A2�/A2n�A1��A0M�A-�^A-�PA,��A,M�A, �A+��A+�;A*5?A'��A&~�A$�9A$5?A#�^A#XA"�RA ��A��A  A/A��A��A�AffAI�A-A�AƨA�AXA$�A"�A��A�A��AA��AVA��AZA�AVA
 �A	�A
ZA
A��Al�A�mAG�A��A9XA|�A|�Al�A�AĜA�9Az�A ��A �@�;d@�x�@�dZ@�hs@��@��@�bN@�O�@�@�@��@�@�D@�Q�@�ȴ@�$�@�(�@�-@�z�@��H@�`B@���@�o@��@�(�@���@�ƨ@׍P@�S�@�K�@�o@��@�M�@���@�9X@���@�J@�hs@υ@�?}@�9X@�"�@�{@��@ɺ^@ɉ7@�O�@�V@�A�@�dZ@���@��@Ə\@�J@�Ĝ@��@�5?@�`B@�&�@�Z@��@��@�|�@�n�@�?}@���@��9@��9@��@���@���@��@�C�@�"�@�5?@��h@���@���@��\@��@�$�@�^5@���@���@�r�@�bN@�Z@�I�@�(�@�1@�9X@�\)@�V@�{@��@���@��@��F@��@�K�@���@�E�@���@�O�@�V@��9@�I�@��@��;@��;@��;@��@� �@�1@�t�@�"�@���@���@�?}@�Ĝ@�9X@�b@���@�\)@��@�v�@�M�@�$�@��@��7@���@��@�1@��m@��
@��m@�1@�b@�1@�b@�1@��@�dZ@�;d@���@���@�~�@��T@�X@�&�@��@�V@�Ĝ@��D@�I�@��@��@��!@�~�@�v�@�V@���@��-@�G�@��@��@�Q�@�A�@�b@��@��@��@��\@�5?@��7@��@��j@��D@�r�@�Z@�A�@� �@��@��m@��m@�  @��m@��
@���@���@�t�@�C�@�;d@�
=@�o@���@���@�=q@�{@�J@�J@��@��-@�`B@��7@�hs@�7L@�j@�A�@�(�@��@��
@���@�|�@�"�@��@��R@���@��+@�M�@�@��@�@��h@��@�`B@�?}@���@��u@�z�@�z�@�r�@�9X@�1@���@�l�@�33@��R@��+@�n�@�E�@�5?@��@�@��@���@��^@��7@��@���@��`@�z�@�  @��F@�t�@�o@���@��+@�n�@�^5@�M�@�{@�@�X@�bN@�1'@��@�(�@�b@��@�(�@��@��
@��@���@�|�@�K�@���@��!@�@��^@��7@��@�P�@w�@g��11111111111111111111111111111111111111111111111111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A߲-A߾wA�ĜA�ĜA�ƨA�ƨA�ĜAߴ9Aߡ�AߓuA�~�A�E�A�?}A�?}A�;dA�7LA�5?A�5?A�5?A�33A�33A�/A�+A� �Aޏ\A��A�|�AыDA�r�A�ffA�|�Aˏ\A�?}AǶFA�S�A��TA���A��;A�A�A���A�K�A��PA�;dA�%A��9A�5?A�VA�VA��+A��HA���A��A��A�{A�G�A�ĜA�ffA�C�A�+A��wA��!A�ĜA��!A�ĜA���A�A�ĜA��hA��jA� �A��`A��A��A�C�A�
=A�{A�G�A�ĜA�ffA�C�A�+A��wA��!A�ĜA��!A�ĜA���A�A�ĜA��hA��jA� �A��`A��A��A�C�A�
=A��TA�"�A�C�A��A���A���A�^5A��^A���A��PA��A��mA���A��A��#A�  A�ĜA�oA��uA���A��jA}�A{�PAzbAx�RAwoAvA�At�As/Aq��Aq�Ap�9Ao��Ao\)Al��Aj�AioAhv�Ag�PAfr�Ab�Aal�A_��A\�\AZJAW�AU�^ATffAS|�AR=qAP�\APE�AP{AO+AK�TAJ{AHI�AF�/AE
=AB�AB  A?�A>��A=��A<VA;oA9�;A9/A7C�A5��A5K�A5A4�\A3��A2�/A2n�A1��A0M�A-�^A-�PA,��A,M�A, �A+��A+�;A*5?A'��A&~�A$�9A$5?A#�^A#XA"�RA ��A��A  A/A��A��A�AffAI�A-A�AƨA�AXA$�A"�A��A�A��AA��AVA��AZA�AVA
 �A	�A
ZA
A��Al�A�mAG�A��A9XA|�A|�Al�A�AĜA�9Az�A ��A �@�;d@�x�@�dZ@�hs@��@��@�bN@�O�@�@�@��@�@�D@�Q�@�ȴ@�$�@�(�@�-@�z�@��H@�`B@���@�o@��@�(�@���@�ƨ@׍P@�S�@�K�@�o@��@�M�@���@�9X@���@�J@�hs@υ@�?}@�9X@�"�@�{@��@ɺ^@ɉ7@�O�@�V@�A�@�dZ@���@��@Ə\@�J@�Ĝ@��@�5?@�`B@�&�@�Z@��@��@�|�@�n�@�?}@���@��9@��9@��@���@���@��@�C�@�"�@�5?@��h@���@���@��\@��@�$�@�^5@���@���@�r�@�bN@�Z@�I�@�(�@�1@�9X@�\)@�V@�{@��@���@��@��F@��@�K�@���@�E�@���@�O�@�V@��9@�I�@��@��;@��;@��;@��@� �@�1@�t�@�"�@���@���@�?}@�Ĝ@�9X@�b@���@�\)@��@�v�@�M�@�$�@��@��7@���@��@�1@��m@��
@��m@�1@�b@�1@�b@�1@��@�dZ@�;d@���@���@�~�@��T@�X@�&�@��@�V@�Ĝ@��D@�I�@��@��@��!@�~�@�v�@�V@���@��-@�G�@��@��@�Q�@�A�@�b@��@��@��@��\@�5?@��7@��@��j@��D@�r�@�Z@�A�@� �@��@��m@��m@�  @��m@��
@���@���@�t�@�C�@�;d@�
=@�o@���@���@�=q@�{@�J@�J@��@��-@�`B@��7@�hs@�7L@�j@�A�@�(�@��@��
@���@�|�@�"�@��@��R@���@��+@�M�@�@��@�@��h@��@�`B@�?}@���@��u@�z�@�z�@�r�@�9X@�1@���@�l�@�33@��R@��+@�n�@�E�@�5?@��@�@��@���@��^@��7@��@���@��`@�z�@�  @��F@�t�@�o@���@��+@�n�@�^5@�M�@�{@�@�X@�bN@�1'@��@�(�@�b@��@�(�@��@��
@��@���@�|�@�K�@���@��!@�@��^@��7@��@�P�@w�@g��11111111111111111111111111111111111111111111111111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BVB_;BcTBffBe`BffBhsBjBl�Bp�Bs�Bx�By�By�By�By�By�By�By�Bz�By�By�Bx�Bw�Bw�B�uB�jB�qB�'B�B�-B��B�NBDB'�B-B2-B>wBC�BG�BI�BN�BM�BN�BM�BN�BT�BVBG�B<jB7LB/B8RB&�B�B�B!�B�B�B{BoB#�B'�B�B1B��B�B�B�B�BB�BB�HB�BB��B��B&�B�B�B!�B�B�B{BoB#�B'�B�B1B��B�B�B�B�BB�BB�HB�BB��B��B��B�qB�B�B�B�B��B�PB�B[#B8RB,B)�B
��B
��B
��B
�PB
�B
|�B
hsB
P�B
5?B
$�B
�B
VB
B	��B	�B	�`B	�;B	�)B	�B	��B	��B	ÖB	�9B	��B	��B	��B	�VB	q�B	k�B	[#B	C�B	9XB	1'B	,B	#�B	�B	�B	uB	oB	{B	�B	�B	
=B	B��B��B�B�mB�ZB�/B�#B�
B��B��BǮBǮB�}B�qB�jB�dB�^B�XB�dB�dB�}B�?B�9B�9B�3B�3B�3B�3B�jB�qB�}B�wB�wB�qB�jB�jB�jB�RB�3B�!B�B��B��B�oB�{B��B�{B�oB�hB�%B�B~�B}�B|�B{�B}�B�B�B�B�B�B� B{�B�B�VB�hB�uB��B�{B�bB�VB�VB�=B�JB�bB��B��B��B��B��B��B��B��B��B�uB�hB�DB�DB�DB�1B�=B�7B�=B�DB�JB�{B�{B��B��B��B��B��B��B��B�B�'B�'B�'B�-B�9B�?B�?B�FB�LB�FB�^B�wB��BBǮBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�#B�#B�#B�B�B�B�/B�fB�B�B�B�B�B�B�B�B��B	B		7B	VB	PB	PB	uB	�B	�B	�B	"�B	$�B	$�B	$�B	$�B	%�B	&�B	-B	<jB	>wB	?}B	@�B	A�B	H�B	Q�B	S�B	YB	ZB	[#B	\)B	_;B	bNB	bNB	bNB	e`B	hsB	hsB	iyB	k�B	m�B	q�B	s�B	t�B	t�B	u�B	w�B	x�B	~�B	�%B	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�?B	�FB	�LB	�^B	�dB	�qB	�qB	�wB	��B	��B	B	ÖB	ŢB	ɺB	��B	��B	��B	��B	��B	�
B	�B	�#B	�/B	�5B	�5B	�;B	�BB	�NB	�TB	�TB	�`B	�fB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
1B
1B
1B
1B
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
JB
PB
VB
VB
\B
\B
bB
bB
hB
hB
B
 vB
+k11111111111111111111111111111111111111111111111111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BVB_;BcTBffBe`BffBhsBjBl�Bp�Bs�Bx�By�By�By�By�By�By�By�Bz�By�By�Bx�Bw�Bw�B�uB�jB�qB�'B�B�-B��B�NBDB'�B-B2-B>wBC�BG�BI�BN�BM�BN�BM�BN�BT�BVBG�B<jB7LB/B8RB&�B�B�B!�B�B�B{BoB#�B'�B�B1B��B�B�B�B�BB�BB�HB�BB��B��B&�B�B�B!�B�B�B{BoB#�B'�B�B1B��B�B�B�B�BB�BB�HB�BB��B��B��B�qB�B�B�B�B��B�PB�B[#B8RB,B)�B
��B
��B
��B
�PB
�B
|�B
hsB
P�B
5?B
$�B
�B
VB
B	��B	�B	�`B	�;B	�)B	�B	��B	��B	ÖB	�9B	��B	��B	��B	�VB	q�B	k�B	[#B	C�B	9XB	1'B	,B	#�B	�B	�B	uB	oB	{B	�B	�B	
=B	B��B��B�B�mB�ZB�/B�#B�
B��B��BǮBǮB�}B�qB�jB�dB�^B�XB�dB�dB�}B�?B�9B�9B�3B�3B�3B�3B�jB�qB�}B�wB�wB�qB�jB�jB�jB�RB�3B�!B�B��B��B�oB�{B��B�{B�oB�hB�%B�B~�B}�B|�B{�B}�B�B�B�B�B�B� B{�B�B�VB�hB�uB��B�{B�bB�VB�VB�=B�JB�bB��B��B��B��B��B��B��B��B��B�uB�hB�DB�DB�DB�1B�=B�7B�=B�DB�JB�{B�{B��B��B��B��B��B��B��B�B�'B�'B�'B�-B�9B�?B�?B�FB�LB�FB�^B�wB��BBǮBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�#B�#B�#B�B�B�B�/B�fB�B�B�B�B�B�B�B�B��B	B		7B	VB	PB	PB	uB	�B	�B	�B	"�B	$�B	$�B	$�B	$�B	%�B	&�B	-B	<jB	>wB	?}B	@�B	A�B	H�B	Q�B	S�B	YB	ZB	[#B	\)B	_;B	bNB	bNB	bNB	e`B	hsB	hsB	iyB	k�B	m�B	q�B	s�B	t�B	t�B	u�B	w�B	x�B	~�B	�%B	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�?B	�FB	�LB	�^B	�dB	�qB	�qB	�wB	��B	��B	B	ÖB	ŢB	ɺB	��B	��B	��B	��B	��B	�
B	�B	�#B	�/B	�5B	�5B	�;B	�BB	�NB	�TB	�TB	�`B	�fB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
1B
1B
1B
1B
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
JB
PB
VB
VB
\B
\B
bB
bB
hB
hB
B
 vB
+k11111111111111111111111111111111111111111111111111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.07 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141518                              AO  ARCAADJP                                                                    20181024141518    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141518  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181024141518  QCF$                G�O�G�O�G�O�4000            