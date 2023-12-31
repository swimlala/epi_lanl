CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:50Z creation      
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
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  JP   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L0   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  S�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [0   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ]   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  fp   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  up   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  wP   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191650  20181005191650  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @פۊn	�1   @פ��z @3�$�/��c�"��`B1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�fC�fC  C�fC"  C$  C&�C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CA�fCD  CF  CH  CJ  CK�fCN  CP  CQ�fCT  CV  CX  CZ  C\  C^�C`  Ca�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�fC��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C��3C��3C�  C�  C��C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C��C��C��C�  C��3C��3C�  C�  C��fC��3C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C��3C�  C��C��C�  C��C�  C��3C��3C��C�  C��C�  C�  C��C�  C��fC�  C��3C�  C�  C��3C��C�  C��3C��C�  C��3C�  C��C�  C�  C�  C��3C��C�  C�  C��C�  C�  C��3C��C�  C�  C��C��C��3C��C�  C��3C��3C�  C�  C�  C�  C��3C�  C��C�  C��C��3C�  D   D � D ��D� DfD�fD��D� DfD� D  D� D  Dy�D  D� D  D� D	  D	� D
  D
�fD  Dy�D  D� D��D�fD  Dy�D  D�fD  D� D  D� D  D� D  D� DfDy�D  D� D  D�fD��D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D   D y�D!fD!y�D"  D"y�D#  D#� D$fD$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+�fD,  D,� D-  D-� D.  D.� D/  D/�fD0  D0�fD1  D1� D2fD2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8fD8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DM��DN�fDO  DOy�DP  DP� DQ  DQ� DR  DRy�DSfDS��DT  DTs3DUfDU� DVfDVy�DV��DWy�DW�3DXy�DX��DY� DZfDZ�fD[  D[� D\  D\y�D]  D]�fD^fD^y�D_fD_y�D`  D`� D`��Da� DbfDb� Db��Dc�fDc��Dd� Dd��De�fDffDfy�Df��Dgs3Dh  Dhy�Dh�3Diy�DjfDj��DkfDk�fDlfDly�Dl��Dmy�Dm��Dn�fDofDo�fDpfDp� Dp��Dq�fDrfDr� Ds  Dsy�Ds��Dty�Du  Du� Du��Dvy�Dw  Dw�fDxfDy�qD�4{D�s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B z�B(z�B0z�B8z�B@z�BHz�BPz�BXz�B`z�Bhz�Bpz�Bxz�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�p�B�=qB�=qB�=qB�=qB�=qC �C�C�C�C�C
�C�C�C�C�C�C�C�CCC�C C"�C$�C&8RC(8RC*�C,�C.�C0�C2�C4�C6�C8�C:�C<8RC>�C@�CBCD�CF�CH�CJ�CLCN�CP�CRCT�CV�CX�CZ�C\�C^8RC`�CbCd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~C��C��C�\C�)C�)C�\C�\C�\C�\C�\C�\C�\C�)C�)C�\C��C��C��C�\C�\C�)C�\C��C�\C�\C�)C�\C�\C�\C�\C�\C�)C�\C�\C�\C�\C�\C�\C��C�\C�)C�)C�\C�)C�)C�)C�\C��C��C�\C�\C���C��C�\C�\C�\C�\C�\C�\C�)C�\C��C��C�\C�\C�\C��C�\C�)C�)C�\C�)C�\C��C��C�)C�\C�)C�\C�\C�)C�\C���C�\C��C�\C�\C��C�)C�\C��C�)C�\C��C�\C�)C�\C�\C�\C��C�)C�\C�\C�)C�\C�\C��C�)C�\C�\C�)C�)C��C�)C�\C��C��C�\C�\C�\C�\C��C�\C�)C�\C�)C��C�\D �D ��DHD��DD�DHD��DD��D�D��D�D�HD�D��D�D��D	�D	��D
�D
�D�D�HD�D��DHD�D�D�HD�D�D�D��D�D��D�D��D�D��DD�HD�D��D�D�DHD��D�D��D�D��D�D��D�D��D�D��D�D��DD��D�D��D �D �HD!D!�HD"�D"�HD#�D#��D$D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+�D,�D,��D-�D-��D.�D.��D/�D/�D0�D0�D1�D1��D2D2�HD3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DGDG��DH�DH��DI�DI��DJ�DJ��DKDK��DL�DL��DM�DM��DNHDN�DO�DO�HDP�DP��DQ�DQ��DR�DR�HDSDS�{DT�DTz�DUDU��DVDV�HDWHDW�HDW��DX�HDYHDY��DZDZ�D[�D[��D\�D\�HD]�D]�D^D^�HD_D_�HD`�D`��DaHDa��DbDb��DcHDc�DdHDd��DeHDe�DfDf�HDgHDgz�Dh�Dh�HDh��Di�HDjDj�{DkDk�DlDl�HDmHDm�HDnHDn�DoDo�DpDp��DqHDq�DrDr��Ds�Ds�HDtHDt�HDu�Du��DvHDv�HDw�Dw�DxDy�D�8RD�w\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ĜA�?}A�=qA��AɶFAɴ9Aɛ�A�VA�~�A�t�A�l�A�$�Aȧ�A�n�A�C�A��A���A���AǴ9Aǣ�Aǟ�AǕ�Aǉ7AǅAǁAǁAǁAǁAǁA�`BA�7LA�
=A���A� �A�Aś�Aĥ�A�\)A��
A�dZA�ĜA�5?A��#A��9A��PA�n�A�E�A���A��RA���A�C�A���A�z�A�=qA���A�bNA���A�oA���A��TA��A��A���A��RA�+A��A�;dA�C�A�VA�%A�&�A�ĜA���A�9XA��A�~�A�l�A�A�A��A���A��+A�
=A��A�E�A�n�A���A��A��^A��A�r�A�r�A�\)A�E�A���A�^5A~bA{�A{O�AzM�Ay&�Av�RAt$�AqAo��An�jAi��Af9XAd=qAa?}A]/AZ��AY��AX�`AUdZAQK�AM?}AK��AI`BAFȴAE�TAD�RA@��A>1A=�-A=O�A<�A<ZA<=qA<1A;�
A;+A:$�A9A6�`A5�A3�TA1��A17LA0(�A/�A-��A,�jA,bNA+��A*�RA(��A&ĜA$��A#�A!`BA 1AC�A�^A��AA�Ax�A  A �A�hA�A�;A�A(�A��AK�A33AȴAVA�Ap�A+A�DA�wA	�TA{A�jAM�A�A?}A��A�A�mA��AS�A��AQ�A�A ��@�dZ@��@�M�@�{@��@��j@��@��#@��7@�%@���@��@���@�&�@��@��`@��j@���@�@�o@�~�@���@���@�x�@�1@��#@�r�@�K�@��@�V@�|�@�;d@�+@�^5@�@��#@�O�@�@ݺ^@��`@���@�`B@أ�@׍P@��@�o@�v�@�E�@�p�@�9X@�
=@Ѻ^@���@϶F@�K�@��H@��@���@�ȴ@·+@Ͳ-@̼j@�j@�dZ@��@�V@ȼj@��
@�t�@�33@��@ư!@�^5@��@�@ě�@Õ�@�ȴ@��@��@�V@��9@��@���@�A�@�1@���@�C�@��H@�{@��T@���@�G�@��j@��@��@��@�A�@�+@�ȴ@��\@�n�@�v�@�5?@�X@��@�  @��@�l�@�33@�~�@�J@���@��h@�X@�&�@���@�V@�x�@��u@�A�@�A�@�l�@��@��
@��;@��@�+@�o@���@�=q@���@��^@��7@�G�@���@���@���@���@�z�@�|�@��y@�ff@���@�/@��9@��w@�|�@�C�@���@�v�@�=q@�J@�@���@�$�@���@�%@���@�A�@��w@�|�@�
=@�E�@�hs@���@�Ĝ@��@��D@�1'@��m@��@��^@�X@�/@�Ĝ@�A�@�|�@�+@��@���@���@�v�@�^5@�J@�7L@���@��@�1'@���@��P@�|�@�t�@�dZ@�S�@�;d@�
=@���@�~�@�-@�@���@�j@�I�@�A�@�9X@��;@�C�@�+@��@��@�~�@�-@��@�@��7@�hs@�?}@��@�Ĝ@��@���@���@��@��@��9@��@��@�1'@��F@�t�@�
=@���@���@�5?@�E�@�^5@���@�X@�Ĝ@��9@��D@�Q�@�9X@�j@���@���@���@���@��u@�Q�@�(�@�b@��;@���@�|�@�S�@�o@�
=@�\)@�|�@�dZ@��y@�^5@�E�@��@��h@���@�r�@�A�@�9X@�  @��w@�C�@��+@�E�@�$�@��@�{@��@�E�@�=q@��#@�x�@�O�@�%@��D@�Q�@�9X@�~�@qk�@c�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ĜA�?}A�=qA��AɶFAɴ9Aɛ�A�VA�~�A�t�A�l�A�$�Aȧ�A�n�A�C�A��A���A���AǴ9Aǣ�Aǟ�AǕ�Aǉ7AǅAǁAǁAǁAǁAǁA�`BA�7LA�
=A���A� �A�Aś�Aĥ�A�\)A��
A�dZA�ĜA�5?A��#A��9A��PA�n�A�E�A���A��RA���A�C�A���A�z�A�=qA���A�bNA���A�oA���A��TA��A��A���A��RA�+A��A�;dA�C�A�VA�%A�&�A�ĜA���A�9XA��A�~�A�l�A�A�A��A���A��+A�
=A��A�E�A�n�A���A��A��^A��A�r�A�r�A�\)A�E�A���A�^5A~bA{�A{O�AzM�Ay&�Av�RAt$�AqAo��An�jAi��Af9XAd=qAa?}A]/AZ��AY��AX�`AUdZAQK�AM?}AK��AI`BAFȴAE�TAD�RA@��A>1A=�-A=O�A<�A<ZA<=qA<1A;�
A;+A:$�A9A6�`A5�A3�TA1��A17LA0(�A/�A-��A,�jA,bNA+��A*�RA(��A&ĜA$��A#�A!`BA 1AC�A�^A��AA�Ax�A  A �A�hA�A�;A�A(�A��AK�A33AȴAVA�Ap�A+A�DA�wA	�TA{A�jAM�A�A?}A��A�A�mA��AS�A��AQ�A�A ��@�dZ@��@�M�@�{@��@��j@��@��#@��7@�%@���@��@���@�&�@��@��`@��j@���@�@�o@�~�@���@���@�x�@�1@��#@�r�@�K�@��@�V@�|�@�;d@�+@�^5@�@��#@�O�@�@ݺ^@��`@���@�`B@أ�@׍P@��@�o@�v�@�E�@�p�@�9X@�
=@Ѻ^@���@϶F@�K�@��H@��@���@�ȴ@·+@Ͳ-@̼j@�j@�dZ@��@�V@ȼj@��
@�t�@�33@��@ư!@�^5@��@�@ě�@Õ�@�ȴ@��@��@�V@��9@��@���@�A�@�1@���@�C�@��H@�{@��T@���@�G�@��j@��@��@��@�A�@�+@�ȴ@��\@�n�@�v�@�5?@�X@��@�  @��@�l�@�33@�~�@�J@���@��h@�X@�&�@���@�V@�x�@��u@�A�@�A�@�l�@��@��
@��;@��@�+@�o@���@�=q@���@��^@��7@�G�@���@���@���@���@�z�@�|�@��y@�ff@���@�/@��9@��w@�|�@�C�@���@�v�@�=q@�J@�@���@�$�@���@�%@���@�A�@��w@�|�@�
=@�E�@�hs@���@�Ĝ@��@��D@�1'@��m@��@��^@�X@�/@�Ĝ@�A�@�|�@�+@��@���@���@�v�@�^5@�J@�7L@���@��@�1'@���@��P@�|�@�t�@�dZ@�S�@�;d@�
=@���@�~�@�-@�@���@�j@�I�@�A�@�9X@��;@�C�@�+@��@��@�~�@�-@��@�@��7@�hs@�?}@��@�Ĝ@��@���@���@��@��@��9@��@��@�1'@��F@�t�@�
=@���@���@�5?@�E�@�^5@���@�X@�Ĝ@��9@��D@�Q�@�9X@�j@���@���@���@���@��u@�Q�@�(�@�b@��;@���@�|�@�S�@�o@�
=@�\)@�|�@�dZ@��y@�^5@�E�@��@��h@���@�r�@�A�@�9X@�  @��w@�C�@��+@�E�@�$�@��@�{@��@�E�@�=q@��#@�x�@�O�@�%@��D@�Q�@�9X@�~�@qk�@c�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��B
��B
�B
�'B
�dB
�;B
�TB
�ZB
��BoB'�BJ�B\)BdZBm�Bq�Bz�B� B�B�1B�=B�DB�JB�PB�PB�VB��B��B��B��B��B��B�B��B��B�9B�FBĜB�B�fB�sB�B�B�BBPB{B0!B?}BO�BO�BL�BF�BC�B?}B0!B�B{B�B	7BbB
=B  B+BuB,B�BuB�B1B��B�`B�;B��B��B�^B�'B�B��BiyB �B
�B
�
B
�'B
��B
��B
�oB
�B
u�B
e`B
P�B
=qB
-B
�B
�B
bB
+B	��B	�ZB	��B	��B	�FB	��B	�%B	x�B	iyB	VB	H�B	@�B	:^B	)�B	�B	%B		7B��B�B�ZB�/BǮB�RB�RB�FB�-B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�oB�bB�\B�DB�+B�B�B�B�B�B�B�B�+B�7B�1B�JB�bB�\B�\B�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�?B�RB�qB��BÖBŢBÖB��B�}B�dB�'B�!B�B�!B�'B�9B�FB�XB�dBÖBŢBȴB��BɺBȴB��B�B�B�B�B�B�B�B�B�B�5B�ZB�mB�mB�B�B�B�B�B�B�B�B��B��B��B	B	%B	B	B	B	+B		7B	JB	VB	VB	\B	\B	VB	VB	VB	uB	�B	�B	�B	�B	"�B	#�B	#�B	$�B	'�B	(�B	'�B	,B	2-B	5?B	7LB	8RB	8RB	8RB	:^B	;dB	<jB	>wB	A�B	G�B	M�B	P�B	R�B	T�B	XB	[#B	_;B	bNB	e`B	hsB	iyB	k�B	l�B	m�B	n�B	n�B	n�B	o�B	p�B	q�B	s�B	x�B	~�B	�B	�B	�B	�B	�B	�7B	�DB	�JB	�PB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�9B	�RB	�^B	�wB	��B	��B	B	ĜB	ÖB	ĜB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�;B	�BB	�BB	�BB	�BB	�HB	�NB	�NB	�TB	�TB	�ZB	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
  B	��B	��B	��B
  B
B
B
B
B
B
B
+B
	7B

=B
DB
JB
PB
VB
\B
\B
bB
bB
hB
�B
$ZB
0�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B
��B
��B
��B
��B
��B
�B
�'B
�dB
�;B
�TB
�ZB
��BoB'�BJ�B\)BdZBm�Bq�Bz�B� B�B�1B�=B�DB�JB�PB�PB�VB��B��B��B��B��B��B�B��B��B�9B�FBĜB�B�fB�sB�B�B�BBPB{B0!B?}BO�BO�BL�BF�BC�B?}B0!B�B{B�B	7BbB
=B  B+BuB,B�BuB�B1B��B�`B�;B��B��B�^B�'B�B��BiyB �B
�B
�
B
�'B
��B
��B
�oB
�B
u�B
e`B
P�B
=qB
-B
�B
�B
bB
+B	��B	�ZB	��B	��B	�FB	��B	�%B	x�B	iyB	VB	H�B	@�B	:^B	)�B	�B	%B		7B��B�B�ZB�/BǮB�RB�RB�FB�-B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�oB�bB�\B�DB�+B�B�B�B�B�B�B�B�+B�7B�1B�JB�bB�\B�\B�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�?B�RB�qB��BÖBŢBÖB��B�}B�dB�'B�!B�B�!B�'B�9B�FB�XB�dBÖBŢBȴB��BɺBȴB��B�B�B�B�B�B�B�B�B�B�5B�ZB�mB�mB�B�B�B�B�B�B�B�B��B��B��B	B	%B	B	B	B	+B		7B	JB	VB	VB	\B	\B	VB	VB	VB	uB	�B	�B	�B	�B	"�B	#�B	#�B	$�B	'�B	(�B	'�B	,B	2-B	5?B	7LB	8RB	8RB	8RB	:^B	;dB	<jB	>wB	A�B	G�B	M�B	P�B	R�B	T�B	XB	[#B	_;B	bNB	e`B	hsB	iyB	k�B	l�B	m�B	n�B	n�B	n�B	o�B	p�B	q�B	s�B	x�B	~�B	�B	�B	�B	�B	�B	�7B	�DB	�JB	�PB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�9B	�RB	�^B	�wB	��B	��B	B	ĜB	ÖB	ĜB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�;B	�BB	�BB	�BB	�BB	�HB	�NB	�NB	�TB	�TB	�ZB	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
  B	��B	��B	��B
  B
B
B
B
B
B
B
+B
	7B

=B
DB
JB
PB
VB
\B
\B
bB
bB
hB
�B
$ZB
0�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.12 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191650                              AO  ARCAADJP                                                                    20181005191650    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191650  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191650  QCF$                G�O�G�O�G�O�8000            