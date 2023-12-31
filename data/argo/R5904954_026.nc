CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:54Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005191654  20181005191654  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׳��7�n1   @׳�8㠖@4���$��c�n��O�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   B   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC�fC  C�C  C  C  C   C"  C$�C&�C(  C*  C,  C.  C/�fC1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj�Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C��C��C��C��C��C�  C��C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��C��C�  C��3C��3C�  C�  C�  C��C��3C��3C�  C��3C�  C�  C��3C��C��C�  C��3C��3C��3C��fC��3C��3C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C��C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��C�  C�  C��C��3C�  C��C��3C��C�  C�  C�  C�  C��C�  C�  C��C��3C�  C��3C��C��3C�  C�  C��C�  C�  C�  C�  C��3C��3C��3C�  C�  C��3D y�D �3D� D  Dy�D  D�fD��D�fD  D� DfD� DfD� DfD� D��D	� D
  D
� D  D�fD  D� D  Dy�D  D�fD  D� D  D� D  D� D  D� D  D� D  D� DfD�fDfDy�D  D� D  D�fD  D� DfD� DfD�fDfD� DfD��DfD�fDfD�fD   D � D!  D!�fD!��D"y�D"��D#� D#��D$� D%fD%��D&  D&� D'  D'� D(fD(� D)  D)y�D)��D*� D+fD+� D,  D,� D-  D-� D.  D.� D/fD/y�D0  D0� D0��D1y�D1��D2� D3fD3� D3��D4� D5fD5�fD6fDB  DBy�DB��DC� DC��DD� DD��DE� DF  DFy�DGfDG� DH  DHy�DI  DI� DJfDJ� DK  DKy�DL  DL� DM  DM� DN  DN� DOfDO� DP  DP�fDQ  DQ� DR  DR�fDS  DS�fDTfDTy�DT�3DU� DV  DV� DW  DW� DXfDX�fDYfDY� DY��DZ� D[  D[�fD[��D\� D\��D]� D^  D^�fD_  D_� D`fD`�fDa  Da�fDb  Dby�Db��Dcy�Dc��Dd� Dd��De� DffDfy�Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dj��Dk�fDk��Dl� Dm  Dmy�Dn  Dn� Do  Do� Dp  Dpy�Dp�3Dqy�Dr  Dr� Dr�3Dsy�Ds��Dt� DufDus3Du��Dv�fDw  Dw� Dw�3Dyw�D�H 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @K�@�@�A�HA"�HAB�HAb�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�B�RB�RB�RB �RB(�RB0�RB8�RB@Q�BH�RBP�RBX�RB`�RBh�RBp�RBx�RB�\)B�\)B�\)B�\)B�\)B�\)B��\B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�(�B�(�B�\)B�\)B�\)B�\)B�\)B�\)C .C.C.C.C.C
.C.C.C.CzCzC.CG�C.C.C.C .C".C$G�C&G�C(.C*.C,.C..C0zC2zC4.C6.C8.C:.C<.C>.C@.CB.CD.CF.CH.CJ.CLzCNzCP.CR.CT.CV.CX.CZ.C\.C^.C`.Cb.Cd.Cf.ChG�CjG�Cl.Cn.CpG�Cr.Ct.Cv.Cx.Cz.C|.C~.C�
C�
C�
C�
C�
C�
C�
=C�
C�
=C�
C�
C�
C�#�C�#�C�
C�
C�#�C�
C�
C�
C�
=C�
C�
C�
C�
C�
=C�
=C�
C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�
C�
C�
C�
C�
C�
C�
=C�
C�
C�#�C�#�C�#�C�
C�
=C�
=C�
C�
C�
C�#�C�
=C�
=C�
C�
=C�
C�
C�
=C�#�C�#�C�
C�
=C�
=C�
=C��pC�
=C�
=C�
C�
C�
C�
=C�
C�
C�
C�
=C�
=C�
C�#�C�
C�#�C�
C�
=C�
C�
C�
C�
C�
C�
C�
C�
=C�#�C�
C�
C�#�C�
=C�
C�#�C�
=C�#�C�
C�
C�
C�
C�#�C�
C�
C�#�C�
=C�
C�
=C�#�C�
=C�
C�
C�#�C�
C�
C�
C�
C�
=C�
=C�
=C�
C�
D D �D ��D��D�D�D�D��DD��D�D��D�D��D�D��D�D��D	D	��D
�D
��D�D��D�D��D�D�D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�D�D��D�D��D�D��D�D��D�D��D�D��D�D�RD�D��D�D��D �D ��D!�D!��D"D"�D#D#��D$D$��D%�D%�RD&�D&��D'�D'��D(�D(��D)�D)�D*D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/�D0�D0��D1D1�D2D2��D3�D3��D4D4��D5�D5��D6�DB�DB�DCDC��DDDD��DEDE��DF�DF�DG�DG��DH�DH�DI�DI��DJ�DJ��DK�DK�DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT�DT��DU��DV�DV��DW�DW��DX�DX��DY�DY��DZDZ��D[�D[��D\D\��D]D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db�DcDc�DdDd��DeDe��Df�Df�Dg�Dg��Dh�Dh�Di�Di��Dj�Dj��DkDk��DlDl��Dm�Dm�Dn�Dn��Do�Do��Dp�Dp�Dp��Dq�Dr�Dr��Dr��Ds�DtDt��Du�Du~�DvDv��Dw�Dw��Dw޸Dy�3D�M�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�hsA�p�AԬA���AԁA�A�v�Aҡ�A�M�A�=qA�-A�(�A�(�A��A���AёhA�=qA�ƨA��`A���A�5?A�ĜA̓uA���A�{AɁA�/A�n�Aơ�A�ZA�C�A�-A�XAě�A�1'A�/Aú^A�A���A�\)A�r�A�A�K�A�XA��jA��wA��7A��A��mA�ffA�K�A�bA��A�jA��A���A�|�A��A��A�(�A��9A��A�VA�JA��mA���A�I�A���A���A���A�A���A�
=A��A� �A�~�A��A�A�A�hsA���A�M�A��7A���A�\)A���A�p�A�Q�A�  A�hsA�A�p�A��;A���A�9XA�S�A���A���A���A�t�A�bA��jA��A}�-Az�Ax��AtVAlȴAkx�Ag�Adv�Aa?}A_��A^VA^JA[��AY��AW�AU�;AUp�AS��AQ��AQ+AP�AO��AO7LAM��AL-AJ��AJA�AIl�AH�RAG��AF�/AE�mAE?}AD1AB5?AAƨA@�A?
=A=�
A;&�A8��A733A4�+A1��A/33A.�\A*jA'�TA%�TA%�PA%��A$��A"{A!`BA!+A!A��A-Ap�AI�A�TA�-A|�AS�A�AO�AȴAI�A�TA+A`BAZA��AVAbNA�jA��A%A	p�A=qA�A��A�9A��A�RAĜA��Ar�Ap�A&�A&�A7LA;dA
=A�`A��A��A�A�/A7LAXAG�AK�AC�A;dA��A �j@�1'@��@�=q@�Z@�S�@�M�@�7@��@��D@�1'@�C�@�hs@�A�@�A�@�Q�@��;@�ȴ@�@��@�G�@�9@�bN@߮@ާ�@�v�@���@݉7@�O�@ݑh@�-@���@݁@�O�@��@ܼj@���@�5?@�@�%@���@��@�+@�{@���@Ѻ^@д9@ϕ�@�^5@́@��/@̛�@�(�@��;@ˮ@��@�~�@��@���@��;@�\)@��@�=q@��@�7L@�%@��/@���@��y@�=q@�`B@�&�@��j@���@�dZ@�\)@�+@���@�-@���@�p�@�`B@�7L@��@���@��u@�9X@��;@��w@��@�
=@��@��\@��T@�%@��@�  @�|�@�"�@��H@��@��H@���@��\@��\@��+@�E�@���@�x�@�`B@�O�@���@���@�z�@�j@�ƨ@�+@��H@���@�~�@�^5@�V@��^@�7L@�O�@�p�@���@��@���@�1'@�9X@�A�@��;@��@��@��u@���@��w@���@��+@�-@���@��@��/@�1'@�33@���@��/@��D@��@��F@���@�C�@��@���@��R@�M�@��@���@���@��7@�G�@�V@��u@�1'@�  @��@��
@��P@�;d@�o@��@���@�~�@�^5@�M�@�=q@�$�@��@�@���@�p�@�X@�/@��`@�Ĝ@�bN@�A�@�9X@�1@���@�t�@�l�@�K�@��@�ȴ@��R@��+@�{@��#@���@��-@���@�x�@�?}@��j@�A�@�b@�b@�  @��
@���@�t�@�S�@�"�@�ȴ@�v�@�^5@�M�@�=q@�$�@�@��7@�?}@�p�@�`B@�`B@���@�1@���@��F@��@�t�@��!@��#@���@���@�hs@�&�@��@�A�@�1@�ƨ@��@��P@�l�@�;d@�"�@�o@���@��+@�5?@�$�@��@�{@�J@��#@��-@��h@��7@��7@�x�@�p�@��@���@��D@a1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�hsA�p�AԬA���AԁA�A�v�Aҡ�A�M�A�=qA�-A�(�A�(�A��A���AёhA�=qA�ƨA��`A���A�5?A�ĜA̓uA���A�{AɁA�/A�n�Aơ�A�ZA�C�A�-A�XAě�A�1'A�/Aú^A�A���A�\)A�r�A�A�K�A�XA��jA��wA��7A��A��mA�ffA�K�A�bA��A�jA��A���A�|�A��A��A�(�A��9A��A�VA�JA��mA���A�I�A���A���A���A�A���A�
=A��A� �A�~�A��A�A�A�hsA���A�M�A��7A���A�\)A���A�p�A�Q�A�  A�hsA�A�p�A��;A���A�9XA�S�A���A���A���A�t�A�bA��jA��A}�-Az�Ax��AtVAlȴAkx�Ag�Adv�Aa?}A_��A^VA^JA[��AY��AW�AU�;AUp�AS��AQ��AQ+AP�AO��AO7LAM��AL-AJ��AJA�AIl�AH�RAG��AF�/AE�mAE?}AD1AB5?AAƨA@�A?
=A=�
A;&�A8��A733A4�+A1��A/33A.�\A*jA'�TA%�TA%�PA%��A$��A"{A!`BA!+A!A��A-Ap�AI�A�TA�-A|�AS�A�AO�AȴAI�A�TA+A`BAZA��AVAbNA�jA��A%A	p�A=qA�A��A�9A��A�RAĜA��Ar�Ap�A&�A&�A7LA;dA
=A�`A��A��A�A�/A7LAXAG�AK�AC�A;dA��A �j@�1'@��@�=q@�Z@�S�@�M�@�7@��@��D@�1'@�C�@�hs@�A�@�A�@�Q�@��;@�ȴ@�@��@�G�@�9@�bN@߮@ާ�@�v�@���@݉7@�O�@ݑh@�-@���@݁@�O�@��@ܼj@���@�5?@�@�%@���@��@�+@�{@���@Ѻ^@д9@ϕ�@�^5@́@��/@̛�@�(�@��;@ˮ@��@�~�@��@���@��;@�\)@��@�=q@��@�7L@�%@��/@���@��y@�=q@�`B@�&�@��j@���@�dZ@�\)@�+@���@�-@���@�p�@�`B@�7L@��@���@��u@�9X@��;@��w@��@�
=@��@��\@��T@�%@��@�  @�|�@�"�@��H@��@��H@���@��\@��\@��+@�E�@���@�x�@�`B@�O�@���@���@�z�@�j@�ƨ@�+@��H@���@�~�@�^5@�V@��^@�7L@�O�@�p�@���@��@���@�1'@�9X@�A�@��;@��@��@��u@���@��w@���@��+@�-@���@��@��/@�1'@�33@���@��/@��D@��@��F@���@�C�@��@���@��R@�M�@��@���@���@��7@�G�@�V@��u@�1'@�  @��@��
@��P@�;d@�o@��@���@�~�@�^5@�M�@�=q@�$�@��@�@���@�p�@�X@�/@��`@�Ĝ@�bN@�A�@�9X@�1@���@�t�@�l�@�K�@��@�ȴ@��R@��+@�{@��#@���@��-@���@�x�@�?}@��j@�A�@�b@�b@�  @��
@���@�t�@�S�@�"�@�ȴ@�v�@�^5@�M�@�=q@�$�@�@��7@�?}@�p�@�`B@�`B@���@�1@���@��F@��@�t�@��!@��#@���@���@�hs@�&�@��@�A�@�1@�ƨ@��@��P@�l�@�;d@�"�@�o@���@��+@�5?@�$�@��@�{@�J@��#@��-@��h@��7@��7@�x�@�p�@��@���@��D@a1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B  B,BH�BN�BP�BP�BP�BQ�BQ�BQ�BQ�BR�BR�BXB\)BbNBffBiyBq�Bs�Bv�Bw�By�B�B�hB��B��B�B�!B�'B�'B�dB��B��B��B�#B�B��B+BuB"�B&�B,B/B7LB7LBB�BO�BQ�BQ�BR�BQ�BP�BVBVBW
B[#B\)BaHB_;B]/BZBQ�BD�B<jB2-B-B'�B�B{B	7B
=B�B+B�yB�}B�VBp�BVB,B
��B
�mB
�;B
��B
ɺB
�FB
��B
r�B
_;B
W
B
O�B
K�B
E�B
9XB
0!B
&�B
#�B
 �B
�B
{B
1B	��B	�`B	�
B	�^B	�VB	�B	s�B	aHB	Q�B	J�B	C�B	@�B	7LB	/B	)�B	#�B	�B	�B	VB	VB	JB	DB	
=B	B��B��B��B�B�B�B�B�yB�mB�TB�/B�)B�B��B��B��B�qB�?B�B�B�B�}BB�!B�B�jB��B��B��B�B�B�#B�#B�B�
B�)B�;B�;B�5B�/B�B�
B�;B�`B�ZB�NB�TB�;B�#B�B��B��B��BƨBŢB��B�jB�}B�}BBŢB��B��B��B��B��B��B��B�B�B�B�B�fB�B�B��B��B��B	  B��B�B�mB�;B��B�XB�B��B��B�B�B�B�B�!B�-B�3B�3B�9B�9B�3B�!B�B��B��B��B��B��B��B��B��B�B�B�'B�^B��B��B��B��BB��B�wB�qB�^B�RB�XB�XB�dB�dB�dB�qB��BĜBƨBǮBǮBɺB��B��B��B��B�
B�/B�;B�;B�TB�fB�fB�mB�mB�mB�yB�B�B��B��B��B��B��B	B	B	
=B	VB	\B	hB	hB	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	%�B	'�B	+B	/B	1'B	49B	6FB	8RB	;dB	<jB	<jB	=qB	>wB	@�B	@�B	A�B	A�B	C�B	E�B	F�B	F�B	H�B	K�B	L�B	N�B	Q�B	S�B	VB	[#B	\)B	]/B	_;B	aHB	bNB	dZB	gmB	jB	n�B	t�B	�B	�B	�B	�1B	�PB	�bB	�hB	�hB	�hB	�hB	�oB	�oB	�oB	�bB	�\B	�\B	�bB	�hB	�oB	�uB	��B	��Bu�B	�?B	�LB	�XB	�dB	�wB	��B	��B	��B	B	B	ÖB	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�5B	�5B	�5B	�;B	�BB	�HB	�HB	�HB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�fB	�mB	�mB	�mB	�sB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�(B
2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
��B  B,BH�BN�BP�BP�BP�BQ�BQ�BQ�BQ�BR�BR�BXB\)BbNBffBiyBq�Bs�Bv�Bw�By�B�B�hB��B��B�B�!B�'B�'B�dB��B��B��B�#B�B��B+BuB"�B&�B,B/B7LB7LBB�BO�BQ�BQ�BR�BQ�BP�BVBVBW
B[#B\)BaHB_;B]/BZBQ�BD�B<jB2-B-B'�B�B{B	7B
=B�B+B�yB�}B�VBp�BVB,B
��B
�mB
�;B
��B
ɺB
�FB
��B
r�B
_;B
W
B
O�B
K�B
E�B
9XB
0!B
&�B
#�B
 �B
�B
{B
1B	��B	�`B	�
B	�^B	�VB	�B	s�B	aHB	Q�B	J�B	C�B	@�B	7LB	/B	)�B	#�B	�B	�B	VB	VB	JB	DB	
=B	B��B��B��B�B�B�B�B�yB�mB�TB�/B�)B�B��B��B��B�qB�?B�B�B�B�}BB�!B�B�jB��B��B��B�B�B�#B�#B�B�
B�)B�;B�;B�5B�/B�B�
B�;B�`B�ZB�NB�TB�;B�#B�B��B��B��BƨBŢB��B�jB�}B�}BBŢB��B��B��B��B��B��B��B�B�B�B�B�fB�B�B��B��B��B	  B��B�B�mB�;B��B�XB�B��B��B�B�B�B�B�!B�-B�3B�3B�9B�9B�3B�!B�B��B��B��B��B��B��B��B��B�B�B�'B�^B��B��B��B��BB��B�wB�qB�^B�RB�XB�XB�dB�dB�dB�qB��BĜBƨBǮBǮBɺB��B��B��B��B�
B�/B�;B�;B�TB�fB�fB�mB�mB�mB�yB�B�B��B��B��B��B��B	B	B	
=B	VB	\B	hB	hB	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	%�B	'�B	+B	/B	1'B	49B	6FB	8RB	;dB	<jB	<jB	=qB	>wB	@�B	@�B	A�B	A�B	C�B	E�B	F�B	F�B	H�B	K�B	L�B	N�B	Q�B	S�B	VB	[#B	\)B	]/B	_;B	aHB	bNB	dZB	gmB	jB	n�B	t�B	�B	�B	�B	�1B	�PB	�bB	�hB	�hB	�hB	�hB	�oB	�oB	�oB	�bB	�\B	�\B	�bB	�hB	�oB	�uB	��B	��Bu�B	�?B	�LB	�XB	�dB	�wB	��B	��B	��B	B	B	ÖB	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�5B	�5B	�5B	�;B	�BB	�HB	�HB	�HB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�fB	�mB	�mB	�mB	�sB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�(B
2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191654                              AO  ARCAADJP                                                                    20181005191654    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191654  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191654  QCF$                G�O�G�O�G�O�8000            