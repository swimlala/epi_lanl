CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:58Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       B@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IL   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       R   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Z�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  a�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       j�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       s�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181005190558  20181005190558  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @���d��1   @���-��@0��t�j�c�r� Ĝ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   B   B   @���@�  @���A   A@  A`  A�  A�  A�  A�33A�  A�  A�33A�33B   B  B  BffB ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�33B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(�C*  C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCd  Cf  Ch�Cj�Cl  Cn  Co�fCq�fCt  Cv  Cx�Cz  C|  C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��3C��3C��C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C��3C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D��D� D  D� D��D� D  D� D  D  D� DfD� D  D� DfD� D  D� D  D�fD  D� D  D� D  D� DfD� D��D� D  Dy�D��D� D  D� D   D � D!  D!� D"  D"� D#fD#� D#��D$� D%  D%�fD&  D&� D'  D'� D(  D(� D)  D)� D*fD*�fD+fD+� D,  D,� D-  D-� D.fD.� D/  D/�fD0fD0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5y�D6  D6� D7  D7� D8  D8� D9  D9�fD:fD:� D;  D;� D<  D<� D=  D=�fD>fD>�fDJ� DKfDK�fDL  DL� DL��DM� DN  DNy�DN��DO� DO��DPy�DP��DQ� DR  DR� DS  DS� DT  DT� DU  DU�fDVfDV� DV��DW�fDX  DX� DY  DY� DZ  DZy�D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dd  Dd�fDefDe� Df  Df� Dg  Dg� Dh  Dh� Dh��Di� Dj  Dj� Dk  Dk� Dk��Dl� Dm  Dm�fDn  Dn� Dn��Do� Dp  Dp�fDq  Dq� Dr  Dr� DsfDs� Ds��Dt� Du  Duy�Du��Dv� Dw  Dw� DxfDx` D�.f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��A\)A$��AD��Ad��A�z�A�z�A�z�A��A�z�A�z�A�A�B=qB	=qB=qB��B!��B)=qB1=qB9=qBA=qBI=qBQ=qBY=qBa=qBi=qBq=qBy=qB���B���B�k�B���B���B���B���B���B�k�B�k�B���B���B���B���B���B���B���BĞ�BȞ�B̞�BО�BԞ�B���Bܞ�B���B䞸B螸B잸B�k�B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\CO\CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$5�C&O\C(h�C*O\C,O\C.O\C05�C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\Cb5�CdO\CfO\Chh�Cjh�ClO\CnO\Cp5�Cr5�CtO\CvO\Cxh�CzO\C|O\C~h�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�4{C�'�C�'�C�'�C��C��C�4{C�4{C�4{C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C��C��C�'�C�'�C�4{C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D ��D�D��DqD��D�D��DqD��D�D��D�D�D��D=D��D�D��D=D��D�D��D�D�=D�D��D�D��D�D��D=D��DqD��D�D�qDqD��D�D��D �D ��D!�D!��D"�D"��D#=D#��D$qD$��D%�D%�=D&�D&��D'�D'��D(�D(��D)�D)��D*=D*�=D+=D+��D,�D,��D-�D-��D.=D.��D/�D/�=D0=D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5�qD6�D6��D7�D7��D8�D8��D9�D9�=D:=D:��D;�D;��D<�D<��D=�D=�=D>=D>�=DJ��DK=DK�=DL�DL��DMqDM��DN�DN�qDOqDO��DPqDP�qDQqDQ��DR�DR��DS�DS��DT�DT��DU�DU�=DV=DV��DWqDW�=DX�DX��DY�DY��DZ�DZ�qD[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc�qDd�Dd�=De=De��Df�Df��Dg�Dg��Dh�Dh��DiqDi��Dj�Dj��Dk�Dk��DlqDl��Dm�Dm�=Dn�Dn��DoqDo��Dp�Dp�=Dq�Dq��Dr�Dr��Ds=Ds��DtqDt��Du�Du�qDvqDv��Dw�Dw��Dx=Dxs�D�8R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�9XA�E�A�=qA�=qA�A�A�E�A�I�A�S�A�ZA�\)A�\)A�`BA�bNA�dZA�hsA�jA�jA�l�A�n�A�p�A�r�A�x�A�|�A�~�A�n�A�bNA�bNA�`BA�bNA�bNA�dZA�ffA�jA�l�A�l�A�l�A�t�A�~�A�bNA� �A�"�A��Aǩ�A�?}A�ZA�{A�|�A��A��A�7LA���A��TA��A��A��;A�bA���A��A���A��A���A�ZA�oA�hsA��A��#A�O�A�dZA���A��+A�ĜA��7A��A�r�A��A�oA��^A���A�$�A�XA���A��hA��mA���A�G�A�1A�&�A���A��A�oA��A��A���A�jA�A��FA�=qA�A�A��`A�ȴA�1A���A���Av$�ApI�Ai?}Ad^5Ab�Aa;dA`jA]�hAW?}AU�7AS��ARffAQ33ANz�ALE�AK��AJ$�AG7LAE�AAA@JA??}A<{A9�;A8�A733A5�FA4��A2�jA0VA.�A.z�A-O�A,�9A+�TA*ZA)x�A(1'A'�hA&�+A$v�A$1A#��A#��A#&�A"�A �/A  �At�A�A=qA&�A��A�A�HA�DA�AoA�A��Az�A��A�`A�AbA�-A"�A��AQ�A��A|�A��A��A1AoA��AjAA
z�A	hsA��A�+A9XA��A`BA�A��AVA��A`BA�HA\)@���@��\@��@��w@��R@��R@���@�(�@��@��@��`@�hs@���@�A�@��H@ꟾ@�7@�(�@��y@�^5@��`@�u@�(�@�ƨ@��@��#@�h@�@�hs@��`@�o@��#@�1@�M�@��@���@�&�@�A�@׾w@�dZ@��@�n�@պ^@��/@ԓu@ӕ�@ҏ\@�x�@���@�9X@���@θR@���@�M�@͙�@�O�@͑h@�+@͉7@�%@�+@�{@ɡ�@���@�I�@�1'@�1'@�t�@ě�@��@�$�@�?}@�`B@��@��`@�%@�7L@�/@�V@���@��/@�r�@��@��@�dZ@���@�M�@���@��7@�/@�r�@�Q�@�A�@��@��P@��@��\@�n�@�{@��^@�x�@��@��/@�I�@���@���@�t�@�;d@��@���@�~�@�V@�-@���@��-@�hs@�V@��9@��@���@�Ĝ@�9X@���@�33@��@��H@�@��@��@�+@�"�@�
=@���@�V@�{@���@�X@�X@���@�Ĝ@�I�@�
=@�v�@�{@���@��^@�?}@�&�@�%@���@�%@�%@���@��@�b@���@�"�@���@�@��P@��P@�ƨ@��@��@��9@��D@�I�@��m@��;@�ƨ@�l�@�o@��!@��@���@��D@��m@�dZ@�S�@���@��\@�@��#@�@�{@���@���@�x�@��@���@��@�r�@�j@�bN@�Q�@��@�ƨ@�|�@�;d@�;d@�@��R@���@�=q@��#@���@��#@�&�@���@��@��/@��@�b@��@���@�S�@�C�@�
=@���@�E�@�$�@��#@���@��7@��7@�X@�&�@���@���@��m@��@��w@��;@�t�@�S�@�C�@��H@���@�~�@�~�@�n�@�V@�E�@�{@���@��@�Ĝ@��D@�Q�@�1@��;@��F@��P@�S�@�"�@{RT1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�9XA�E�A�=qA�=qA�A�A�E�A�I�A�S�A�ZA�\)A�\)A�`BA�bNA�dZA�hsA�jA�jA�l�A�n�A�p�A�r�A�x�A�|�A�~�A�n�A�bNA�bNA�`BA�bNA�bNA�dZA�ffA�jA�l�A�l�A�l�A�t�A�~�A�bNA� �A�"�A��Aǩ�A�?}A�ZA�{A�|�A��A��A�7LA���A��TA��A��A��;A�bA���A��A���A��A���A�ZA�oA�hsA��A��#A�O�A�dZA���A��+A�ĜA��7A��A�r�A��A�oA��^A���A�$�A�XA���A��hA��mA���A�G�A�1A�&�A���A��A�oA��A��A���A�jA�A��FA�=qA�A�A��`A�ȴA�1A���A���Av$�ApI�Ai?}Ad^5Ab�Aa;dA`jA]�hAW?}AU�7AS��ARffAQ33ANz�ALE�AK��AJ$�AG7LAE�AAA@JA??}A<{A9�;A8�A733A5�FA4��A2�jA0VA.�A.z�A-O�A,�9A+�TA*ZA)x�A(1'A'�hA&�+A$v�A$1A#��A#��A#&�A"�A �/A  �At�A�A=qA&�A��A�A�HA�DA�AoA�A��Az�A��A�`A�AbA�-A"�A��AQ�A��A|�A��A��A1AoA��AjAA
z�A	hsA��A�+A9XA��A`BA�A��AVA��A`BA�HA\)@���@��\@��@��w@��R@��R@���@�(�@��@��@��`@�hs@���@�A�@��H@ꟾ@�7@�(�@��y@�^5@��`@�u@�(�@�ƨ@��@��#@�h@�@�hs@��`@�o@��#@�1@�M�@��@���@�&�@�A�@׾w@�dZ@��@�n�@պ^@��/@ԓu@ӕ�@ҏ\@�x�@���@�9X@���@θR@���@�M�@͙�@�O�@͑h@�+@͉7@�%@�+@�{@ɡ�@���@�I�@�1'@�1'@�t�@ě�@��@�$�@�?}@�`B@��@��`@�%@�7L@�/@�V@���@��/@�r�@��@��@�dZ@���@�M�@���@��7@�/@�r�@�Q�@�A�@��@��P@��@��\@�n�@�{@��^@�x�@��@��/@�I�@���@���@�t�@�;d@��@���@�~�@�V@�-@���@��-@�hs@�V@��9@��@���@�Ĝ@�9X@���@�33@��@��H@�@��@��@�+@�"�@�
=@���@�V@�{@���@�X@�X@���@�Ĝ@�I�@�
=@�v�@�{@���@��^@�?}@�&�@�%@���@�%@�%@���@��@�b@���@�"�@���@�@��P@��P@�ƨ@��@��@��9@��D@�I�@��m@��;@�ƨ@�l�@�o@��!@��@���@��D@��m@�dZ@�S�@���@��\@�@��#@�@�{@���@���@�x�@��@���@��@�r�@�j@�bN@�Q�@��@�ƨ@�|�@�;d@�;d@�@��R@���@�=q@��#@���@��#@�&�@���@��@��/@��@�b@��@���@�S�@�C�@�
=@���@�E�@�$�@��#@���@��7@��7@�X@�&�@���@���@��m@��@��w@��;@�t�@�S�@�C�@��H@���@�~�@�~�@�n�@�V@�E�@�{@���@��@�Ĝ@��D@�Q�@�1@��;@��F@��P@�S�@�"�@{RT1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BÖBÖBÖBÖBÖBÖBÖBÖBÖBĜBŢBƨBƨBȴB��B��B��B��B�B�)B�;B�yB�B�B	B	%B	%B	%B	%B	+B	1B	1B	
=B	PB	oB	�B	-B	o�B	�;B
� B
�wB
��B.BD�BdZBk�B{�B�=B��B�B�
B�B��BB1BVB{B�B'�B1'B8RB<jBK�BO�BK�BL�BH�BC�B@�B<jB7LB33B+B#�B�B��B�ZB��B�RB��B�+Bk�BS�BF�B:^B�B%B
��B
�#B
�XB
�'B
��B
��B
��B
��B
��B
��B
�=B
hsB
:^B
(�B
�B	�B	��B	m�B	>wB	$�B	�B	oB	
=B��B�;B��B��BǮBB�qB�^B�FB�'B��B��B��B��B�uB�oB�uB�uB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�FBB��B��B��BɺBƨB��B�qB�}B��BȴB��B��B��B��BɺBĜBBB�}B�jB�qB��B��B��B��B��BB��B��B��BȴB��B��B��B��B��BƨB�}B�}BǮBƨBɺB��B��B��BɺBŢB��B��B��BĜB��BBĜBǮB��B��B��B�
B�
B�B�)B�NB�ZB�fB�mB�yB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B	+B	
=B	DB	hB	oB	bB	bB	uB	"�B	!�B	&�B	.B	0!B	1'B	5?B	9XB	>wB	@�B	A�B	>wB	;dB	9XB	M�B	p�B	y�B	� B	� B	�B	�B	�B	�B	�7B	�=B	�DB	�DB	�JB	�PB	�\B	�oB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�B	�!B	�-B	�?B	�LB	�RB	�^B	�jB	�jB	�wB	��B	ÖB	ŢB	ƨB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�`B	�mB	J�B	��B	��B	��B	��B	��B
  B
B
B
B
B
B	��B	��B	��B	��B	��B
  B
  B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
	7B

=B

=B

=B
JB
PB
DB
DB
DB
JB
PB
VB
VB
VB
\B
\B
\B
\B
bB
bB
bB
hB
oB
uB
{B
{B
{B
uB
uB
uB
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
!�B
4�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222442222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 BÖBÖBÖBÖBÖBÖBÖBÖBÖBĜBŢBƨBƨBȴB��B��B��B��B�B�)B�;B�yB�B�B	B	%B	%B	%B	%B	+B	1B	1B	
=B	PB	oB	�B	-B	o�B	�;B
� B
�wB
��B.BD�BdZBk�B{�B�=B��B�B�
B�B��BB1BVB{B�B'�B1'B8RB<jBK�BO�BK�BL�BH�BC�B@�B<jB7LB33B+B#�B�B��B�ZB��B�RB��B�+Bk�BS�BF�B:^B�B%B
��B
�#B
�XB
�'B
��B
��B
��B
��B
��B
��B
�=B
hsB
:^B
(�B
�B	�B	��B	m�B	>wB	$�B	�B	oB	
=B��B�;B��B��BǮBB�qB�^B�FB�'B��B��B��B��B�uB�oB�uB�uB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�FBB��B��B��BɺBƨB��B�qB�}B��BȴB��B��B��B��BɺBĜBBB�}B�jB�qB��B��B��B��B��BB��B��B��BȴB��B��B��B��B��BƨB�}B�}BǮBƨBɺB��B��B��BɺBŢB��B��B��BĜB��BBĜBǮB��B��B��B�
B�
B�B�)B�NB�ZB�fB�mB�yB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B	+B	
=B	DB	hB	oB	bB	bB	uB	"�B	!�B	&�B	.B	0!B	1'B	5?B	9XB	>wB	@�B	A�B	>wB	;dB	9XB	M�B	p�B	y�B	� B	� B	�B	�B	�B	�B	�7B	�=B	�DB	�DB	�JB	�PB	�\B	�oB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�B	�!B	�-B	�?B	�LB	�RB	�^B	�jB	�jB	�wB	��B	ÖB	ŢB	ƨB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�`B	�mB	J�B	��B	��B	��B	��B	��B
  B
B
B
B
B
B	��B	��B	��B	��B	��B
  B
  B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
	7B

=B

=B

=B
JB
PB
DB
DB
DB
JB
PB
VB
VB
VB
\B
\B
\B
\B
bB
bB
bB
hB
oB
uB
{B
{B
{B
uB
uB
uB
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
!�B
4�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222442222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190558                              AO  ARCAADJP                                                                    20181005190558    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190558  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190558  QCF$                G�O�G�O�G�O�C000            