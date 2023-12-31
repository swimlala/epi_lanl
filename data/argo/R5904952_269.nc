CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:06Z creation      
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190606  20181005190606  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @������1   @���ww�v@26ȴ9X�c�Z�11   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @@  @�  @�  A   A   A@  A`  A�  A���A���A�  A�  A�  A�  A���B   B  B  B��B   B(ffB0  B8  B@  BH  BP  BX  B`ffBhffBp  Bw��B�  B�  B�  B�  B�33B�33B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C;�fC=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCc�fCf  Ch  Cj  Cl  Cn  Cp  Cq�fCs�fCv  Cx  Cz  C|  C~  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C��C��C�  C�  C�  C��3C��3C��3C�  C��C�  C�  C�  C��C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3D   D � D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D	  D	� D	��D
� D  D� D  D� D  D� D  D�fD  Dy�D  D�fD  D� DfD� D��D� D  D� D  D� D  D� D  D� D  D�fDfD�fD  D� D  D�fD  D� DfD�fD  Dy�D  D� D   D �fD!fD!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,�fD-  D-� D.  D.� D/fD/� D0  D0� D1  D1� D2fD2�fD3fD3�fD4  D4y�D4��D5� D6  D6�fD7fD7� D8  D8� D8��D9� D:  D:�fD;  D;� D<  D<� D=  D=� D>fD>�fD?  D?y�D@  D@� DA  DAy�DB  DB� DB��DCy�DD  DD�fDEfDE� DF  DF� DG  DG� DH  DH� DI  DI� DI��DJy�DK  DK� DL  DL� DM  DM� DN  DN�fDO  DO� DO��DPy�DP��DQy�DR  DR� DR��DSy�DS��DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ�fD[  D[y�D\  D\� D]fD]� D]��D^� D_  D_� D`  D`� Da  Da�fDb  Db� Dc  Dcy�Dd  Dd�fDe  De� Df  Df� DgfDg� Dh  Dh� Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dm  Dm�fDnfDn�fDofDo� Dp  Dp�fDqfDq�fDr  Dr� Ds  Dsy�Ds��Dty�Du  Du� DvfDv�fDw  Dwy�Dw�fDy��D�IH111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @R�\@�G�@�G�A��A$��AD��Ad��A�Q�A��A��A�Q�A�Q�A�Q�A�Q�A��B(�B	(�B(�BB!(�B)�\B1(�B9(�BA(�BI(�BQ(�BY(�Ba�\Bi�\Bq(�BxB��{B��{B��{B��{B�ǮB�ǮB��{B��{B��{B�ǮB��{B��{B��{B��{B��{B��{B�aHBĔ{BȔ{B̔{BД{BԔ{Bؔ{Bܔ{B��{B�{B�{B�{B�{B��{B��{B��{C J=Cc�CJ=CJ=CJ=C
J=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C J=C"J=C$J=C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4J=C6c�C8J=C:J=C<0�C>0�C@J=CBJ=CDJ=CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=Cb0�Cd0�CfJ=ChJ=CjJ=ClJ=CnJ=CpJ=Cr0�Ct0�CvJ=CxJ=CzJ=C|J=C~J=C�%C�1�C�1�C�1�C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�RC�RC�RC�RC�RC�%C�%C�%C�RC�%C�%C�%C�%C�1�C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�RC�RC�%C�%C�%C�%C�%C�%C�%C�1�C�1�C�%C�%C�1�C�%C�%C�1�C�1�C�%C�%C�%C�RC�RC�RC�%C�1�C�%C�%C�%C�1�C�1�C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�RD �D ��D�D��D�D��D�D�)D�D��D�D��D�D��D�D��D�D��D	�D	��D
)D
��D�D��D�D��D�D��D�D��D�D�)D�D��D�D��D�D��D)D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�)D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4�)D5)D5��D6�D6��D7�D7��D8�D8��D9)D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?�)D@�D@��DA�DA�)DB�DB��DC)DC�)DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ)DJ�)DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP)DP�)DQ)DQ�)DR�DR��DS)DS�)DT)DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[�)D\�D\��D]�D]��D^)D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc�)Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds�)Dt)Dt�)Du�Du��Dv�Dv��Dw�Dw�)Dw��Dy�\D�R�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�I�A�S�A�VA�S�A�VA�ZA�\)A�`BA�bNA�dZA�bNA�bNA�bNA�dZA�ffA�hsA�hsA�hsA�hsA�l�A�l�A�l�A�l�A�n�A�l�A�l�A�hsA�r�A�r�A�t�A�t�A�v�A�p�A�hsA�XA�ZA�`BA�;dA�C�A�;dA��A���A���Ạ�A��A�x�A��/A��;A�n�AăA�
=A�=qA�$�A�~�A�z�A�l�A�jA�1'A���A��A��A�1A���A��\A���A�v�A��A�M�A��9A�O�A���A�~�A���A�`BA�E�A���A��`A��!A��A�bNA�;dA�VA�C�A�A�A���A���A�(�A���A�ffA�?}A���A��\A���A� �A��A��A��A��/A��A��A�;dA�S�A�  A��yA���A�O�A�I�A~=qA}\)A{t�Ax1'Av�+At�!As�-AsoAr�!Aq;dAn��Am�Ak`BAhI�Ac��Aa?}A^�uAZ�ATbAN�uAKO�AJbAFn�AD�+AA��AA7LA@A�A>jA<bNA9\)A41'A1�TA1�A/��A/7LA.ȴA,�A*�uA'��A'"�A';dA'G�A'K�A&�A&�uA%S�A$bNA$1A#VA"=qA!�A!/A ȴA ^5AƨA
=A�Ar�AE�AA�A��AVA�A�AƨA��A�yA��AA"�Ar�A�FA�7A%A��AC�A�A��A�RA\)AA
�!A	ƨA	C�A��AZA�TA?}AbNA�A�A��A�A`BAA�AA -A 1'@��+@��!@���@�5?@�M�@���A -@��
@�K�@��@�t�@�{@��`@���@�
=@��@�I�@�bN@�dZ@��@��@���@��@�b@��;@�9X@�o@���@�9@��@�`B@��/@�(�@땁@�dZ@�bN@�V@���@��m@�ƨ@��y@���@�ȴ@�I�@�E�@◍@�j@�@��y@�9@�t�@ް!@���@�n�@އ+@�~�@�ff@�S�@�9X@�b@��u@�Ĝ@��@��u@�I�@߾w@�C�@�ȴ@�
=@�J@ݙ�@�O�@���@ܼj@�r�@�I�@�j@�t�@�V@ف@�V@ج@�Q�@�ƨ@�o@�-@ա�@��@�I�@ӥ�@ҸR@���@�G�@��@�Q�@϶F@��@�ȴ@�n�@�$�@Ͳ-@�Z@��@�@�ȴ@ʧ�@�V@���@��@ȓu@�z�@ȴ9@�Ĝ@���@���@���@���@�Ĝ@ȴ9@��;@ƸR@�v�@�^5@�=q@�J@���@�`B@�Z@Ý�@�33@�o@�
=@��y@���@���@¸R@���@��@��j@��u@�z�@�b@��@�|�@�S�@��@�M�@�5?@��@��h@�V@���@�bN@� �@�ƨ@�\)@�;d@�@�5?@�hs@��@�A�@�  @�|�@�ff@���@�p�@���@��u@� �@��@���@���@�C�@��!@���@�`B@���@�j@��w@�+@�ȴ@�{@��@��^@�x�@�&�@���@�Ĝ@���@�Q�@�9X@�9X@�9X@�(�@��w@��H@�n�@���@�%@�A�@���@�\)@��@�
=@��y@���@�~�@�-@��T@�p�@�V@�Ĝ@�Z@�Q�@��D@��u@��u@��D@��D@�Z@�1@���@�|�@��R@���@��h@�O�@�V@��/@��`@��`@���@��D@�r�@�dZ@�o@��!@�^5@�-@��#@���@�?}@��`@��9@�j@�  @��P@�;d@�+@��@�+@�C�@��@���@���@�n�@�E�@�@��-@��7@�G�@�Ĝ@�Z@�1'@��@��@��w@��@�t�@�S�@��@��H@�J@��@�`B@�?}@���@���@�Ĝ@��j@��9@�bN@���@�|�@�t�@�\)@�33@�
=@���@���@���@�n/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�I�A�S�A�VA�S�A�VA�ZA�\)A�`BA�bNA�dZA�bNA�bNA�bNA�dZA�ffA�hsA�hsA�hsA�hsA�l�A�l�A�l�A�l�A�n�A�l�A�l�A�hsA�r�A�r�A�t�A�t�A�v�A�p�A�hsA�XA�ZA�`BA�;dA�C�A�;dA��A���A���Ạ�A��A�x�A��/A��;A�n�AăA�
=A�=qA�$�A�~�A�z�A�l�A�jA�1'A���A��A��A�1A���A��\A���A�v�A��A�M�A��9A�O�A���A�~�A���A�`BA�E�A���A��`A��!A��A�bNA�;dA�VA�C�A�A�A���A���A�(�A���A�ffA�?}A���A��\A���A� �A��A��A��A��/A��A��A�;dA�S�A�  A��yA���A�O�A�I�A~=qA}\)A{t�Ax1'Av�+At�!As�-AsoAr�!Aq;dAn��Am�Ak`BAhI�Ac��Aa?}A^�uAZ�ATbAN�uAKO�AJbAFn�AD�+AA��AA7LA@A�A>jA<bNA9\)A41'A1�TA1�A/��A/7LA.ȴA,�A*�uA'��A'"�A';dA'G�A'K�A&�A&�uA%S�A$bNA$1A#VA"=qA!�A!/A ȴA ^5AƨA
=A�Ar�AE�AA�A��AVA�A�AƨA��A�yA��AA"�Ar�A�FA�7A%A��AC�A�A��A�RA\)AA
�!A	ƨA	C�A��AZA�TA?}AbNA�A�A��A�A`BAA�AA -A 1'@��+@��!@���@�5?@�M�@���A -@��
@�K�@��@�t�@�{@��`@���@�
=@��@�I�@�bN@�dZ@��@��@���@��@�b@��;@�9X@�o@���@�9@��@�`B@��/@�(�@땁@�dZ@�bN@�V@���@��m@�ƨ@��y@���@�ȴ@�I�@�E�@◍@�j@�@��y@�9@�t�@ް!@���@�n�@އ+@�~�@�ff@�S�@�9X@�b@��u@�Ĝ@��@��u@�I�@߾w@�C�@�ȴ@�
=@�J@ݙ�@�O�@���@ܼj@�r�@�I�@�j@�t�@�V@ف@�V@ج@�Q�@�ƨ@�o@�-@ա�@��@�I�@ӥ�@ҸR@���@�G�@��@�Q�@϶F@��@�ȴ@�n�@�$�@Ͳ-@�Z@��@�@�ȴ@ʧ�@�V@���@��@ȓu@�z�@ȴ9@�Ĝ@���@���@���@���@�Ĝ@ȴ9@��;@ƸR@�v�@�^5@�=q@�J@���@�`B@�Z@Ý�@�33@�o@�
=@��y@���@���@¸R@���@��@��j@��u@�z�@�b@��@�|�@�S�@��@�M�@�5?@��@��h@�V@���@�bN@� �@�ƨ@�\)@�;d@�@�5?@�hs@��@�A�@�  @�|�@�ff@���@�p�@���@��u@� �@��@���@���@�C�@��!@���@�`B@���@�j@��w@�+@�ȴ@�{@��@��^@�x�@�&�@���@�Ĝ@���@�Q�@�9X@�9X@�9X@�(�@��w@��H@�n�@���@�%@�A�@���@�\)@��@�
=@��y@���@�~�@�-@��T@�p�@�V@�Ĝ@�Z@�Q�@��D@��u@��u@��D@��D@�Z@�1@���@�|�@��R@���@��h@�O�@�V@��/@��`@��`@���@��D@�r�@�dZ@�o@��!@�^5@�-@��#@���@�?}@��`@��9@�j@�  @��P@�;d@�+@��@�+@�C�@��@���@���@�n�@�E�@�@��-@��7@�G�@�Ĝ@�Z@�1'@��@��@��w@��@�t�@�S�@��@��H@�J@��@�`B@�?}@���@���@�Ĝ@��j@��9@�bN@���@�|�@�t�@�\)@�33@�
=@���@���@���@�n/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bs�Bu�Bu�Bt�Bu�Bu�Bu�Bu�Bu�Bu�Bv�Bu�Bu�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bu�Bu�Bt�Bv�Bv�Bv�Bv�Bv�Bu�Bt�Br�Br�Br�Bo�Bp�Bn�BjBffBbNB`BB]/B�JB	  B	#�B	]/B	�=B	��B	�
B	��B
  B
 �B
<jB
[#B
�B
�\B
�JB
��B�BD�B;dB�B$�B@�BZB`BBbNBbNBaHB^5BI�B>wB6FB-B'�B33BC�BH�Bo�B�+B�VB�hB��B��B��B��B|�BJ�B!�BB
�
B
�VB
q�B
cTB
P�B
6FB
uB	��B	�B	��B	��B	�B	p�B	cTB	`BB	k�B	|�B	z�B	t�B	p�B	n�B	l�B	iyB	cTB	XB	O�B	@�B	-B	uB	B�B�BB��B��B��BɺB�wB�^B�^B�dB�^B�FB�3B�B�LB�}B�dB�LB�FB�9B�3B�!B�B�B�RB�RB�RB�XB�RB�LB�RB�RB�XB�XB��BÖBBĜB��B��B��B��B�#B�BB�BB�ZB�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	B��B��B	  B��B	B	1B	DB	PB	VB	oB	�B	�B	�B	�B	�B	#�B	�B	�B	�B	&�B	#�B	,B	/B	7LB	;dB	@�B	XB	[#B	ZB	YB	XB	T�B	XB	]/B	^5B	\)B	\)B	]/B	\)B	YB	YB	ZB	[#B	]/B	`BB	hsB	ffB	`BB	_;B	_;B	l�B	o�B	l�B	k�B	m�B	z�B	�B	�B	~�B	� B	{�B	q�B	e`B	YB	Q�B	]/B	XB	aHB	s�B	l�B	gmB	gmB	dZB	iyB	jB	k�B	k�B	x�B	�+B	�7B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�9B	�FB	�LB	�XB	�dB	�qB	�}B	��B	��B	B	B	B	B	B	B	B	B	ÖB	ŢB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	�B	�B	�#B	�#B	�)B	�)B	�5B	�5B	�;B	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�HB	�BB	�BB	�BB	�NB	�ZB	�ZB	�`B	�`B	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B
  B	��B
  B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
1B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B
1B
+B
%B
%B
B
%B
%B
%B
%B
%B
%B
%B
%B
%B
%B
%B
+B
1B
	B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  Bs�Bu�Bu�Bt�Bu�Bu�Bu�Bu�Bu�Bu�Bv�Bu�Bu�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bu�Bu�Bt�Bv�Bv�Bv�Bv�Bv�Bu�Bt�Br�Br�Br�Bo�Bp�Bn�BjBffBbNB`BB]/B�JB	  B	#�B	]/B	�=B	��B	�
B	��B
  B
 �B
<jB
[#B
�B
�\B
�JB
��B�BD�B;dB�B$�B@�BZB`BBbNBbNBaHB^5BI�B>wB6FB-B'�B33BC�BH�Bo�B�+B�VB�hB��B��B��B��B|�BJ�B!�BB
�
B
�VB
q�B
cTB
P�B
6FB
uB	��B	�B	��B	��B	�B	p�B	cTB	`BB	k�B	|�B	z�B	t�B	p�B	n�B	l�B	iyB	cTB	XB	O�B	@�B	-B	uB	B�B�BB��B��B��BɺB�wB�^B�^B�dB�^B�FB�3B�B�LB�}B�dB�LB�FB�9B�3B�!B�B�B�RB�RB�RB�XB�RB�LB�RB�RB�XB�XB��BÖBBĜB��B��B��B��B�#B�BB�BB�ZB�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	B��B��B	  B��B	B	1B	DB	PB	VB	oB	�B	�B	�B	�B	�B	#�B	�B	�B	�B	&�B	#�B	,B	/B	7LB	;dB	@�B	XB	[#B	ZB	YB	XB	T�B	XB	]/B	^5B	\)B	\)B	]/B	\)B	YB	YB	ZB	[#B	]/B	`BB	hsB	ffB	`BB	_;B	_;B	l�B	o�B	l�B	k�B	m�B	z�B	�B	�B	~�B	� B	{�B	q�B	e`B	YB	Q�B	]/B	XB	aHB	s�B	l�B	gmB	gmB	dZB	iyB	jB	k�B	k�B	x�B	�+B	�7B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�9B	�FB	�LB	�XB	�dB	�qB	�}B	��B	��B	B	B	B	B	B	B	B	B	ÖB	ŢB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	�B	�B	�#B	�#B	�)B	�)B	�5B	�5B	�;B	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�HB	�BB	�BB	�BB	�NB	�ZB	�ZB	�`B	�`B	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B
  B	��B
  B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
1B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B
1B
+B
%B
%B
B
%B
%B
%B
%B
%B
%B
%B
%B
%B
%B
%B
+B
1B
	B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190606                              AO  ARCAADJP                                                                    20181005190606    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190606  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190606  QCF$                G�O�G�O�G�O�8000            