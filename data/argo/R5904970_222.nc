CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:15:45Z creation      
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
resolution        =���   axis      Z           9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ^    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        `   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   h(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        j0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        rP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   zp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        |x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024141545  20181024141545  5904970 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6785                            2B  A   APEX                            7726                            111215                          846 @��Z��81   @���>�&@6�bM���d ě��T1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �B   B   B   @�  @�  @���A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"fD"�fD#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=�fD>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DB��DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DQ��DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�fDy�qD�:�D�w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@q�@���@�Az�A<z�A\z�A|z�A�p�A�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\BÏ\BǏ\Bˏ\BϏ\Bӏ\B׏\Bۏ\Bߏ\B�\B�\B�\B�\B�\B��\B��\B��\CǮCǮCǮCǮC	ǮCǮCǮCǮCǮCǮCǮCǮCǮCǮCǮCǮC!ǮC#ǮC%ǮC'ǮC)ǮC+ǮC-ǮC/ǮC1ǮC3ǮC5ǮC7ǮC9ǮC;ǮC=ǮC?ǮCAǮCCǮCEǮCGǮCIǮCKǮCMǮCOǮCQǮCSǮCUǮCWǮCYǮC[ǮC]ǮC_ǮCaǮCcǮCeǮCgǮCiǮCkǮCmǮCoǮCqǮCsǮCuǮCwǮCyǮC{ǮC}ǮCǮC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D q�D ��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D	q�D	��D
q�D
��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D�RDq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D q�D ��D!q�D!�RD"xRD"��D#q�D#��D$q�D$��D%q�D%��D&q�D&��D'q�D'��D(q�D(��D)q�D)��D*q�D*��D+q�D+��D,q�D,��D-q�D-��D.q�D.��D/q�D/��D0q�D0��D1q�D1��D2q�D2��D3q�D3��D4q�D4��D5q�D5��D6q�D6��D7q�D7��D8q�D-��D.q�D.��D/q�D/��D0q�D0��D1q�D1��D2q�D2��D3q�D3��D4q�D4��D5q�D5��D6q�D6��D7q�D7��D8q�D8��D9q�D9��D:q�D:��D;q�D;��D<q�D<��D=xRD=��D>q�D>��D?q�D?��D@q�D@��DAq�DA��DBq�DB�DCq�DC��DDq�DD��DEq�DE��DFq�DF��DGq�DG��DHq�DH��DIq�DI��DJq�DJ��DKq�DK��DLq�DL��DMq�DM��DNq�DN��DOq�DO��DPq�DP��DQq�DQ�DRq�DR��DSq�DS��DTq�DT��DUq�DU��DVq�DV��DWq�DW��DXq�DX��DYq�DY��DZq�DZ��D[q�D[��D\q�D\��D]q�D]��D^q�D^��D_q�D_��D`q�D`��Daq�Da��Dbq�Db��Dcq�Dc��Ddq�Dd��Deq�De��Dfq�Df��Dgq�Dg��Dhq�Dh��Diq�Di��Djq�Dj��Dkq�Dk��Dlq�Dl��Dmq�Dm��Dnq�Dn��Doq�Do��Dpq�Dp��Dqq�Dq��Drq�Dr��Dsq�Ds��Dtq�Dt��Duq�Du��Dvq�Dv��Dwq�Dw�RDy�]D�3�D�p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�dZA�p�A�|�A��+A��A��A��A��A��PA��DA��DA��PA��DA��DA��PA�n�A�bNA�^5A�E�A�  A���A�ȴA���A�bNA��A���A���A��A�t�A�x�A��7A��DA��7A�z�A�S�A�=qA�?}A�9XA��HA�~�A�=qA�^5A�K�A��^A��A�?}A��A���A�oA��uA�O�A�+A��A�  A��RA�l�A�&�A��wA���A��A�`BA�ZA�O�A��!A��/A���A��A��jA��DA�9XA�(�A��/A���A��A��FA��hA�A�A��FA��A��FA�jA�"�A��A��#A��A���A��hA��A��A�?}A���A��A��A�  A��A�A��A���A�&�A�ƨA�  A���A��A���A�+A���A�VAp�A+A~5?A}�Azz�Ax�RAt��Ar�\Aq�Ap��Ao�Ao�Am��Al�Ai`BAh$�AgdZAfȴAd��Ac?}Ab=qA`�uA_x�A]\)A[K�AZVAYdZAX5?AW�7AWS�AW+AUATbAS�#AS��AR~�AQx�AQ&�AP�jAOdZAN=qANJAM�hAMXAK�TAJVAHA�AF�`AFn�AE�#AEdZAD�uAC��AA�mAA%A>�9A<�!A<�uA<A�A:ffA8bA7dZA7\)A7K�A7+A6 �A4  A2ĜA1�A01'A/t�A/`BA.ȴA,��A+33A)�wA(��A'��A%��A$�A#�A"�A A��A7LA��AE�AZA��AG�A��A�A1'A�HA��AE�A33A
�A
��A
�A
9XA
1A	�wA	�A	;dA�AVA  A�A`BA�HA�mA�jA1A�hA �y@��@�n�@�7L@���@�l�@�E�@���@��H@��T@�O�@�@�r�@�9X@�\)@��@�n�@���@�Z@�\)@�n�@�@��#@�O�@��m@���@�|�@�V@�@��@�bN@ߍP@�K�@���@ܼj@�dZ@١�@؋D@�\)@պ^@҇+@�@Ѻ^@�G�@Л�@ύP@��#@ˮ@�+@�@Ǯ@Ɨ�@�M�@���@�@ċD@�l�@�$�@�@�X@��y@��H@��T@��P@��@��!@�@�
=@�@�^5@�&�@���@�?}@��@� �@��^@�M�@��R@��;@��F@�33@���@��^@�Z@�@�5?@��-@�Q�@��@��y@��
@�1'@�1@��@�K�@�n�@�n�@�=q@�5?@�@���@��@�X@��@�(�@�"�@�^5@�@��@��7@�O�@���@�r�@���@�\)@�o@��R@�ff@��#@��7@��@�z�@���@�\)@�C�@�;d@���@���@�33@�bN@�b@��;@���@��@�`B@��9@���@��H@�~�@��7@�G�@�&�@��@�r�@��F@��@�C�@�;d@���@���@�33@�bN@�b@��;@���@��@�`B@��9@���@��H@�~�@��7@�G�@�&�@��@�r�@��F@��@�~�@�5?@���@�p�@�V@��@�`B@���@�@��@���@�&�@�K�@�"�@�o@�
=@���@�dZ@���@��y@�S�@�;d@�K�@�l�@�+@���@�=q@�E�@�^5@�5?@���@���@���@��-@���@�@��#@�@���@�p�@��m@��P@�|�@�dZ@�33@��y@�V@�J@�?}@���@��@�1'@��P@�ȴ@�v�@�V@�5?@�J@�{@�@���@��@���@�@��-@���@��7@�?}@�/@��@�V@���@��`@�Ĝ@��@�I�@�|�@�S�@�S�@�K�@�C�@�33@�33@�33@�"�@��@�o@��H@��R@���@�~�@�ff@�=q@��@�@��@��#@��#@��#@�@���@�hs@�hs@�`B@�?}@�X@�`B@�?}@�%@���@���@�bN@�A�@��
@���@���@��P@��P@���@�|�@�K�@�"�@���@�V@�J@��7@���@��@xG@h11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�dZA�p�A�|�A��+A��A��A��A��A��PA��DA��DA��PA��DA��DA��PA�n�A�bNA�^5A�E�A�  A���A�ȴA���A�bNA��A���A���A��A�t�A�x�A��7A��DA��7A�z�A�S�A�=qA�?}A�9XA��HA�~�A�=qA�^5A�K�A��^A��A�?}A��A���A�oA��uA�O�A�+A��A�  A��RA�l�A�&�A��wA���A��A�`BA�ZA�O�A��!A��/A���A��A��jA��DA�9XA�(�A��/A���A��A��FA��hA�A�A��FA��A��FA�jA�"�A��A��#A��A���A��hA��A��A�?}A���A��A��A�  A��A�A��A���A�&�A�ƨA�  A���A��A���A�+A���A�VAp�A+A~5?A}�Azz�Ax�RAt��Ar�\Aq�Ap��Ao�Ao�Am��Al�Ai`BAh$�AgdZAfȴAd��Ac?}Ab=qA`�uA_x�A]\)A[K�AZVAYdZAX5?AW�7AWS�AW+AUATbAS�#AS��AR~�AQx�AQ&�AP�jAOdZAN=qANJAM�hAMXAK�TAJVAHA�AF�`AFn�AE�#AEdZAD�uAC��AA�mAA%A>�9A<�!A<�uA<A�A:ffA8bA7dZA7\)A7K�A7+A6 �A4  A2ĜA1�A01'A/t�A/`BA.ȴA,��A+33A)�wA(��A'��A%��A$�A#�A"�A A��A7LA��AE�AZA��AG�A��A�A1'A�HA��AE�A33A
�A
��A
�A
9XA
1A	�wA	�A	;dA�AVA  A�A`BA�HA�mA�jA1A�hA �y@��@�n�@�7L@���@�l�@�E�@���@��H@��T@�O�@�@�r�@�9X@�\)@��@�n�@���@�Z@�\)@�n�@�@��#@�O�@��m@���@�|�@�V@�@��@�bN@ߍP@�K�@���@ܼj@�dZ@١�@؋D@�\)@պ^@҇+@�@Ѻ^@�G�@Л�@ύP@��#@ˮ@�+@�@Ǯ@Ɨ�@�M�@���@�@ċD@�l�@�$�@�@�X@��y@��H@��T@��P@��@��!@�@�
=@�@�^5@�&�@���@�?}@��@� �@��^@�M�@��R@��;@��F@�33@���@��^@�Z@�@�5?@��-@�Q�@��@��y@��
@�1'@�1@��@�K�@�n�@�n�@�=q@�5?@�@���@��@�X@��@�(�@�"�@�^5@�@��@��7@�O�@���@�r�@���@�\)@�o@��R@�ff@��#@��7@��@�z�@���@�\)@�C�@�;d@���@���@�33@�bN@�b@��;@���@��@�`B@��9@���@��H@�~�@��7@�G�@�&�@��@�r�@��F@��@�C�@�;d@���@���@�33@�bN@�b@��;@���@��@�`B@��9@���@��H@�~�@��7@�G�@�&�@��@�r�@��F@��@�~�@�5?@���@�p�@�V@��@�`B@���@�@��@���@�&�@�K�@�"�@�o@�
=@���@�dZ@���@��y@�S�@�;d@�K�@�l�@�+@���@�=q@�E�@�^5@�5?@���@���@���@��-@���@�@��#@�@���@�p�@��m@��P@�|�@�dZ@�33@��y@�V@�J@�?}@���@��@�1'@��P@�ȴ@�v�@�V@�5?@�J@�{@�@���@��@���@�@��-@���@��7@�?}@�/@��@�V@���@��`@�Ĝ@��@�I�@�|�@�S�@�S�@�K�@�C�@�33@�33@�33@�"�@��@�o@��H@��R@���@�~�@�ff@�=q@��@�@��@��#@��#@��#@�@���@�hs@�hs@�`B@�?}@�X@�`B@�?}@�%@���@���@�bN@�A�@��
@���@���@��P@��P@���@�|�@�K�@�"�@���@�V@�J@��7@���@��@xG@h11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BZB[#B\)B_;BhsBo�Bp�Bx�B�1B��B��B�B�LB�wB��B�mB�mB�mB�fB�TB�NB�ZB�sB��B%B
=B{B�B�B�B�B�B�B �B!�B"�B"�B"�B#�B$�B#�B"�B�BbB+B��B�)BȴBB�XB��B�%B� B}�Bz�Bu�Bp�Bn�BiyBZBJ�BG�BC�B>wB9XB5?B0!B$�B�BPBB��B��B�B�fB�5B�BŢB�dB�?B��B�VB�7BbNBF�B2-BbB
�B
��B
��B
�dB
��B
��B
��B
��B
�VB
z�B
n�B
W
B
C�B
;dB
7LB
/B
)�B
�B
�B
  B	��B	�B	�mB	�)B	��B	ǮB	�}B	�FB	��B	��B	�uB	�PB	�%B	� B	}�B	z�B	t�B	jB	iyB	hsB	cTB	]/B	YB	W
B	Q�B	G�B	E�B	B�B	@�B	:^B	0!B	&�B	�B	�B	�B	{B	\B	PB	B��B�B�sB�fB�ZB�5B�B��B��B��B��B��BB�wB�RB�9B�'B�!B�B��B��B��B��B�uB�\B�7B�%B�B{�Bu�Bq�Bl�BgmBcTB^5BVBT�BT�BP�BO�BN�BN�BK�BK�BK�BK�BJ�BJ�BJ�BI�BH�BH�BH�BF�BG�BC�BD�BD�BB�B@�B>wB<jB7LB2-B.B-B-B-B0!B0!B.B.B/B.B.B/B/B/B0!B0!B0!B0!B/B/B.B0!B2-B2-B2-B33B33B33B49B33B33B49B49B6FB6FB6FB7LB9XB9XB9XB9XB9XB8RB<jB;dB:^B:^B=qB<jB<jB<jB<jB@�BC�BI�BJ�BI�BO�BXB^5BVBQ�BQ�BW
B\)Bm�Bw�Bv�Bq�BhsBy�B�B�%B�\B��B�B�B�B�!B�!B�B�B�B�B�B�B�dBȴB��B�B�B�B�ZB�`B�yB�yB�B�B�B�B�B��B��B��B	B	+B	1B	DB	VB	hB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	%�B	(�B	-B	7LB	:^B	;dB	9XB	:^B	:^B	:^B	<jB	=qB	=qB	<jB	<jB	=qB	=qB	@�B	A�B	8RB	 �B	"�B	%�B	(�B	-B	7LB	:^B	;dB	9XB	:^B	:^B	:^B	<jB	=qB	=qB	<jB	<jB	=qB	=qB	@�B	A�B	C�B	F�B	H�B	K�B	K�B	K�B	L�B	M�B	O�B	Q�B	VB	XB	[#B	ZB	[#B	[#B	[#B	`BB	bNB	bNB	dZB	iyB	jB	jB	k�B	k�B	k�B	m�B	n�B	o�B	p�B	s�B	t�B	u�B	v�B	w�B	w�B	w�B	w�B	x�B	}�B	�B	�B	�B	�B	�B	�+B	�=B	�JB	�\B	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�9B	�?B	�FB	�FB	�FB	�LB	�LB	�LB	�LB	�RB	�XB	�dB	�jB	�qB	�}B	��B	B	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�5B	�BB	�BB	�BB	�BB	�BB	�NB	�ZB	�ZB	�ZB	�`B	�`B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�pB
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BZB[#B\)B_;BhsBo�Bp�Bx�B�1B��B��B�B�LB�wB��B�mB�mB�mB�fB�TB�NB�ZB�sB��B%B
=B{B�B�B�B�B�B�B �B!�B"�B"�B"�B#�B$�B#�B"�B�BbB+B��B�)BȴBB�XB��B�%B� B}�Bz�Bu�Bp�Bn�BiyBZBJ�BG�BC�B>wB9XB5?B0!B$�B�BPBB��B��B�B�fB�5B�BŢB�dB�?B��B�VB�7BbNBF�B2-BbB
�B
��B
��B
�dB
��B
��B
��B
��B
�VB
z�B
n�B
W
B
C�B
;dB
7LB
/B
)�B
�B
�B
  B	��B	�B	�mB	�)B	��B	ǮB	�}B	�FB	��B	��B	�uB	�PB	�%B	� B	}�B	z�B	t�B	jB	iyB	hsB	cTB	]/B	YB	W
B	Q�B	G�B	E�B	B�B	@�B	:^B	0!B	&�B	�B	�B	�B	{B	\B	PB	B��B�B�sB�fB�ZB�5B�B��B��B��B��B��BB�wB�RB�9B�'B�!B�B��B��B��B��B�uB�\B�7B�%B�B{�Bu�Bq�Bl�BgmBcTB^5BVBT�BT�BP�BO�BN�BN�BK�BK�BK�BK�BJ�BJ�BJ�BI�BH�BH�BH�BF�BG�BC�BD�BD�BB�B@�B>wB<jB7LB2-B.B-B-B-B0!B0!B.B.B/B.B.B/B/B/B0!B0!B0!B0!B/B/B.B0!B2-B2-B2-B33B33B33B49B33B33B49B49B6FB6FB6FB7LB9XB9XB9XB9XB9XB8RB<jB;dB:^B:^B=qB<jB<jB<jB<jB@�BC�BI�BJ�BI�BO�BXB^5BVBQ�BQ�BW
B\)Bm�Bw�Bv�Bq�BhsBy�B�B�%B�\B��B�B�B�B�!B�!B�B�B�B�B�B�B�dBȴB��B�B�B�B�ZB�`B�yB�yB�B�B�B�B�B��B��B��B	B	+B	1B	DB	VB	hB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	%�B	(�B	-B	7LB	:^B	;dB	9XB	:^B	:^B	:^B	<jB	=qB	=qB	<jB	<jB	=qB	=qB	@�B	A�B	8RB	 �B	"�B	%�B	(�B	-B	7LB	:^B	;dB	9XB	:^B	:^B	:^B	<jB	=qB	=qB	<jB	<jB	=qB	=qB	@�B	A�B	C�B	F�B	H�B	K�B	K�B	K�B	L�B	M�B	O�B	Q�B	VB	XB	[#B	ZB	[#B	[#B	[#B	`BB	bNB	bNB	dZB	iyB	jB	jB	k�B	k�B	k�B	m�B	n�B	o�B	p�B	s�B	t�B	u�B	v�B	w�B	w�B	w�B	w�B	x�B	}�B	�B	�B	�B	�B	�B	�+B	�=B	�JB	�\B	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�9B	�?B	�FB	�FB	�FB	�LB	�LB	�LB	�LB	�RB	�XB	�dB	�jB	�qB	�}B	��B	B	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�5B	�BB	�BB	�BB	�BB	�BB	�NB	�ZB	�ZB	�ZB	�`B	�`B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�pB
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.22 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141545                              AO  ARCAADJP                                                                    20181024141545    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141545  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141545  QCF$                G�O�G�O�G�O�4000            