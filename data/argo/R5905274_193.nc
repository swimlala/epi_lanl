CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:41Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7p   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8P   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8X   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8\   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8`   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9,   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9l   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           9t   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            9�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [h   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  cL   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Ѭ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � @   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � gx   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �p   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �x   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230426223241  20230426223241  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�"���Lt@�"���Lt11  @�"���P@�"���P@0����-�@0����-��dI.�Se�dI.�Se11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  AA  ?k�?��H@@  @�  @��R@�p�@�  A ��A�RA\)A,��AAG�AaG�A���A�Q�A���A���A�Q�A�  A߮A�  B (�B�
B  BQ�B Q�B(Q�B0(�B8(�B@  BG�
BO�BX  B`  Bh  Bp  Bw�B�  B�{B��B��B�  B��B��B�{B��B�  B�(�B�{B�(�B�  B��B��B��B��
B��B�{B�{B��B�  B�(�B��B�B��
B�{B�{B�{B�(�B��B�C��C  C  C��C	��C
=C
=C
=C  C  C  C��C��C�HC�C�C"  C$
=C&
=C'��C)��C+��C.  C/�C1�C3��C6  C8  C:
=C<
=C>
=C@  CA��CD
=CF�CH�CJ�CL
=CN  CO��CQ�HCT  CV�CW��CY��C\  C^
=C`{Cb  Cc��Ce��Cg�Cj  Cl
=Cn  Cp  Cr  Cs��Cu�Cx  Cz
=C|  C~  C�C�
=C���C���C�  C���C�C�  C���C�  C�  C���C�C�
=C�
=C�\C�C���C��C���C�  C���C�C�  C�C�C�  C�C�C�  C�  C�  C�  C���C���C�C�  C�  C�C�
=C�C���C�  C�C�  C�C�\C�
=C�C�C�  C�  C�  C�  C�  C���C���C�  C�
=C�  C�C�
=C�C�C�
=C�C���C���C���C��C���C�  C�  C�C�
=C�C�C�C�  C���C�  C�C�  C���C�C�
=C���C���C���C���C���C���C���C���C�  C�C���C���C���C�  C�C�  C�
=C���C�C���C�  C�  C���C���C�  C�  C�  C�  C�
=C�  C�  C���C���C�  C�  C���C���C�C�
=C�C�
=C�D   D ��D�Dz�D�qD��D  Dz�D�qD}qD�qD� D��D}qD�D��D  D� D	D	�D
  D
� D
�qD� DD��D�qD� D  D��D�D� D�D��DD�DD� D  D��D  D� D�qD}qD  D� D��Dz�D  D�D�D}qD  D��D��D� D  D}qD  D}qD  D}qD�qD��D �D ��D!�D!��D!�qD"� D#D#}qD#�qD$��D%D%� D%�qD&� D&�qD'z�D'�qD(}qD(�qD)}qD*  D*� D+  D+}qD,  D,��D-�D-��D.�D.� D.��D/� D0�D0� D0�qD1z�D1�qD2� D3  D3}qD3��D4� D5�D5}qD6  D6� D6�qD7� D8�D8� D8�qD9z�D9�qD:� D;  D;��D<�D<�D=D=��D>�D>�D?D?�D@D@� D@�qDA��DB�DB� DC  DC��DD  DD� DE�DE}qDF  DF��DGDG��DH�DH��DI�DI��DJDJ��DJ�qDK}qDK�qDL}qDL�qDM� DN�DN� DN�qDOz�DO�qDP��DQDQ�DR  DR}qDS�DS�DT�DT� DT�qDU� DV�DV� DW  DW}qDX  DX� DX�qDY��DZ  DZ}qD[  D[� D[�qD\� D]  D]� D]�qD^� D_D_� D_�qD`� DaDa�Db  Db� Dc  Dc}qDd  Dd� De  De�Df  Df� Dg�Dgz�Dg�qDh� Dh�qDi� Dj  Dj� Dk  Dk� Dl�Dl�Dm�Dm� Dm�qDn}qDn��Do� Dp�Dp}qDq�Dq��Dq�qDr� Ds�Ds� Dt�Dt��Dt�qDu}qDu�qDv� Dw  Dw�Dx�Dx}qDy  Dy��Dy�qDz}qD{�D{z�D{�RD|z�D|�qD}}qD}��D~}qD~�qD� D�HD�AHD��HD���D�HD�B�D��HD��HD��D�@ D�}qD���D�HD�B�D��HD���D���D�@ D���D�D�HD�@ D�� D�� D���D�AHD���D�� D�  D�B�D�� D���D�HD�>�D�}qD�� D���D�<)D�~�D��HD��D�AHD�~�D���D�  D�@ D�� D��qD���D�AHD��HD�� D���D�>�D�� D�� D�HD�B�D�� D��qD��)D�@ D���D�D��D�AHD�~�D���D���D�=qD�� D��HD��D�B�D�~�D���D���D�@ D�~�D���D���D�=qD�}qD��)D�  D�AHD��HD��qD���D�AHD�� D��qD�HD�AHD�~�D��qD���D�AHD��HD���D���D�=qD��HD�D��D�@ D�~�D��qD���D�B�D��HD�� D���D�>�D�~�D�D�HD�>�D���D���D���D�>�D�|)D�� D���D�=qD���D���D�  D�AHD��HD�� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�  D�AHD�� D���D�HD�AHD��HD��HD�  D�>�D�~�D�� D��qD�>�D�~�D�� D�  D�AHD�~�D��HD�  D�>�D�� D��HD�  D�>�D�~�D�� D�  D�AHD�� D��HD���D�>�D��HD��HD��D�@ D��HD�� D��qD�@ D�� D�D�HD�AHD�� D���D���D�@ D��HD�D�HD�AHD�� D�� D�  D�@ D�� D��HD�  D�>�D�� D�� D���D�@ D�� D�� D�  D�AHD���D��HD�  D�>�D�}qD���D���D�>�D�~�D�� D�  D�@ D��HD�� D���D�B�D�~�D���D�  D�@ D��HD�D�HD�@ D�� D�� D�  D�AHD���D��HD�  D�>�D�� D�� D���D�@ D�}qD��qD�HD�AHD�� D���D���D�>�D�~�D���D���D�@ D�� D���D���D�AHD��HD�� D���D�>�D D�� D��D�>�DÀ D��HD�  D�AHDāHD�� D�  D�>�DŁHD�� D���D�@ Dƀ D�� D�HD�AHD�~�DǽqD�  D�B�DȂ�D�� D��qD�=qD�~�Dɾ�D�  D�@ DʁHD�� D��qD�=qD�~�D˾�D���D�=qD�~�D̾�D���D�<)D�}qD;�D���D�>�D�~�Dξ�D���D�@ D�~�D��HD��D�@ DЁHD�� D���D�@ DсHD�� D�HD�@ D�~�DҾ�D�HD�>�DӀ D��HD�  D�>�DԀ DԾ�D�  D�>�DՀ D�� D�HD�@ Dր D�D�HD�@ DׁHD׾�D�HD�@ D�}qD�� D�  D�>�Dـ D�� D���D�@ DځHD�� D��D�>�Dۀ D���D�  D�@ D܁HDܾ�D�HD�=qD݀ DݽqD�  D�AHD�~�D޾�D�  D�AHD߀ D�� D��qD�@ D�~�DྸD���D�@ DႏD�� D�HD�@ D�|)D�� D���D�AHD� D�D�  D�>�D� D�� D�  D�C�D� D��HD�HD�<)D悏D�D��D�>�D�HD�� D�  D�AHD�HD�D�  D�AHD�HD��HD�HD�AHD� D��HD�HD�AHD�HD�� D�  D�AHD삏D��HD�HD�@ D� D�� D�  D�AHD�HD�� D�  D�B�D� DﾸD��qD�>�D�� D�� D���D�@ D� D��HD���D�>�D�~�D��HD�  D�=qD�~�D�� D�  D�AHD� D���D�HD�AHD�� D��HD�  D�>�D�}qD��HD�  D�@ D�}qD�� D�  D�>�D�� D�� D���D�>�D�~�D��HD�  D�B�D�� D��?�?aG�?�=q?�Q�?�G�?��H@
=@(��@E�@Y��@n{@��
@���@��R@���@�Q�@��@�33@�  @�\)@�(�Az�A(�A33A��A   A%A.{A4z�A:=qA@��AG
=AMp�ATz�A[�Aa�Ah��Ap  Ax��A�Q�A��
A��A�33A��RA��\A�ffA���A�p�A���A�z�A�  A��A�\)A�33A�\)A��
A��A��
AǮA�33A�ffAҏ\A�ffA�=qA�ffA�=qA�RA�\A�RA�=qA�ffA���A�(�B   BB�Bp�B\)B	�B
�\B  Bp�B
=B��B=qB�B�BffB�
B�B�\B�B��B=qB�B z�B!��B"�\B#�B$��B%B&�HB'�
B(��B)B*�HB+�B,��B-��B.�\B/\)B0Q�B1p�B2=qB2�HB3�
B4��B5�B6�HB7�
B8��B9B:�RB;�B<Q�B=p�B>=qB?33B@(�BA�BB{BC\)BD(�BEG�BFffBG�BHQ�BIG�BJ=qBK33BL(�BL��BM�BN�RBO�
BP��BQp�BRffBS33BT  BT��BU��BV�RBW�BXQ�BYp�BZ{B[
=B\  B\��B]B^�\B_�
Ba�Bb{Bc
=Bd  Be�Bf=qBg33Bh  Bh��BiBj�RBk�
Bl��Bm��Bn�HBp(�Bq�Br{Bs
=Bt(�Bu�Bv{Bv�RBw�Bxz�Byp�Bz=qB{33B|(�B}�B~=qB�B�(�B��RB�33B��B�  B�ffB���B�p�B��B�ffB���B��B�(�B��RB�33B���B�  B��\B�
=B���B�(�B��HB�\)B��B�ffB��HB�\)B��
B�ffB���B��B�=qB��RB�G�B��B�(�B��RB�33B��
B�z�B�
=B���B�=qB��RB��B���B�(�B���B�\)B�  B��\B���B�\)B�  B�z�B��B�B�ffB���B�p�B�{B�z�B�
=B��B�{B���B�\)B�  B�z�B��HB�\)B�  B��RB�G�B��
B�Q�B��RB�\)B��
B�ffB��B��
B�ffB���B�G�B��B�ffB�33B��
B�ffB�
=B�p�B��B�z�B�33B��B��\B�
=B���B�(�B��HB���B�Q�B��RB�33B�B�z�B�33B��B��\B�
=B��B�(�B��RB�p�B�(�B���B�\)B�B�ffB�
=B��
B�z�B��HBŅB�{B���BǅB�=qBȣ�B�33BɮBʏ\B�33B��
B�Q�B��HB�p�B�=qB���Bϙ�B�(�B��HB�G�B��
B�Q�B�33B��B�z�B�
=BՅB�(�B���Bי�B�Q�B���B�G�B��Bڣ�B�p�B�{B܏\B��B�Bޏ\B�G�B��B��\B�
=BᙚB�=qB�
=B�B�z�B���B�B�(�B���B�B�=qB�RB�\)B�{B��HB�B�=qB��B�G�B��
B�RB�\)B�  B�z�B��B��B�\B��B�B�=qB���B��
B�z�B���B��B�=qB���B��B�ffB���B��B�Q�B�
=B���B�(�B���B��C (�C z�C C�C�C�
C{CffC��C(�C�C�RC
=Cp�C��C�C\)C��C�Cz�CC
=CQ�C��C(�CQ�C��C	�C	p�C	�C	��C
p�C
��C  CQ�CC�C\)C��C��CffC�RC�C33C��C�C(�C�C�C=qCz�C��C=qC�\C��C(�C�\C�
C�Cz�C�HC�Cp�C�HC=qCp�CC33C�\CC�C�C��C{CffC�HC(�Cp�C�RC33Cz�CC{C�CC{C�\C�
C(�C��C�C33C�C  CG�C��C 
=C ffC ��C!{C!z�C!�C"
=C"z�C"�RC#
=C#�C#�
C${C$p�C$�C%(�C%z�C&  C&Q�C&��C'�C'p�C'�C(33C(�C(��C)G�C)�\C)��C*ffC*��C+{C+z�C+C,{C,�\C,�HC-Q�C-�C-��C.\)C.��C/
=C/p�C/�HC0(�C0�\C1  C1G�C1C2�C2p�C2�HC3Q�C3��C4{C4z�C4��C5=qC5��C5�C6ffC6C7{C7��C7�
C8G�C8C9
=C9z�C9�HC:(�C:�C:��C;Q�C;��C<�C<p�C=  C=G�C=��C>�C>p�C>C?G�C?�\C@  C@p�C@�RCA33CA��CA�CB\)CBCC
=CC�CC��CD(�CD�RCE  CEz�CE��CF(�CF�CG  CGz�CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                             ?k�?��H@@  @�  @��R@�p�@�  A ��A�RA\)A,��AAG�AaG�A���A�Q�A���A���A�Q�A�  A߮A�  B (�B�
B  BQ�B Q�B(Q�B0(�B8(�B@  BG�
BO�BX  B`  Bh  Bp  Bw�B�  B�{B��B��B�  B��B��B�{B��B�  B�(�B�{B�(�B�  B��B��B��B��
B��B�{B�{B��B�  B�(�B��B�B��
B�{B�{B�{B�(�B��B�C��C  C  C��C	��C
=C
=C
=C  C  C  C��C��C�HC�C�C"  C$
=C&
=C'��C)��C+��C.  C/�C1�C3��C6  C8  C:
=C<
=C>
=C@  CA��CD
=CF�CH�CJ�CL
=CN  CO��CQ�HCT  CV�CW��CY��C\  C^
=C`{Cb  Cc��Ce��Cg�Cj  Cl
=Cn  Cp  Cr  Cs��Cu�Cx  Cz
=C|  C~  C�C�
=C���C���C�  C���C�C�  C���C�  C�  C���C�C�
=C�
=C�\C�C���C��C���C�  C���C�C�  C�C�C�  C�C�C�  C�  C�  C�  C���C���C�C�  C�  C�C�
=C�C���C�  C�C�  C�C�\C�
=C�C�C�  C�  C�  C�  C�  C���C���C�  C�
=C�  C�C�
=C�C�C�
=C�C���C���C���C��C���C�  C�  C�C�
=C�C�C�C�  C���C�  C�C�  C���C�C�
=C���C���C���C���C���C���C���C���C�  C�C���C���C���C�  C�C�  C�
=C���C�C���C�  C�  C���C���C�  C�  C�  C�  C�
=C�  C�  C���C���C�  C�  C���C���C�C�
=C�C�
=C�D   D ��D�Dz�D�qD��D  Dz�D�qD}qD�qD� D��D}qD�D��D  D� D	D	�D
  D
� D
�qD� DD��D�qD� D  D��D�D� D�D��DD�DD� D  D��D  D� D�qD}qD  D� D��Dz�D  D�D�D}qD  D��D��D� D  D}qD  D}qD  D}qD�qD��D �D ��D!�D!��D!�qD"� D#D#}qD#�qD$��D%D%� D%�qD&� D&�qD'z�D'�qD(}qD(�qD)}qD*  D*� D+  D+}qD,  D,��D-�D-��D.�D.� D.��D/� D0�D0� D0�qD1z�D1�qD2� D3  D3}qD3��D4� D5�D5}qD6  D6� D6�qD7� D8�D8� D8�qD9z�D9�qD:� D;  D;��D<�D<�D=D=��D>�D>�D?D?�D@D@� D@�qDA��DB�DB� DC  DC��DD  DD� DE�DE}qDF  DF��DGDG��DH�DH��DI�DI��DJDJ��DJ�qDK}qDK�qDL}qDL�qDM� DN�DN� DN�qDOz�DO�qDP��DQDQ�DR  DR}qDS�DS�DT�DT� DT�qDU� DV�DV� DW  DW}qDX  DX� DX�qDY��DZ  DZ}qD[  D[� D[�qD\� D]  D]� D]�qD^� D_D_� D_�qD`� DaDa�Db  Db� Dc  Dc}qDd  Dd� De  De�Df  Df� Dg�Dgz�Dg�qDh� Dh�qDi� Dj  Dj� Dk  Dk� Dl�Dl�Dm�Dm� Dm�qDn}qDn��Do� Dp�Dp}qDq�Dq��Dq�qDr� Ds�Ds� Dt�Dt��Dt�qDu}qDu�qDv� Dw  Dw�Dx�Dx}qDy  Dy��Dy�qDz}qD{�D{z�D{�RD|z�D|�qD}}qD}��D~}qD~�qD� D�HD�AHD��HD���D�HD�B�D��HD��HD��D�@ D�}qD���D�HD�B�D��HD���D���D�@ D���D�D�HD�@ D�� D�� D���D�AHD���D�� D�  D�B�D�� D���D�HD�>�D�}qD�� D���D�<)D�~�D��HD��D�AHD�~�D���D�  D�@ D�� D��qD���D�AHD��HD�� D���D�>�D�� D�� D�HD�B�D�� D��qD��)D�@ D���D�D��D�AHD�~�D���D���D�=qD�� D��HD��D�B�D�~�D���D���D�@ D�~�D���D���D�=qD�}qD��)D�  D�AHD��HD��qD���D�AHD�� D��qD�HD�AHD�~�D��qD���D�AHD��HD���D���D�=qD��HD�D��D�@ D�~�D��qD���D�B�D��HD�� D���D�>�D�~�D�D�HD�>�D���D���D���D�>�D�|)D�� D���D�=qD���D���D�  D�AHD��HD�� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�  D�AHD�� D���D�HD�AHD��HD��HD�  D�>�D�~�D�� D��qD�>�D�~�D�� D�  D�AHD�~�D��HD�  D�>�D�� D��HD�  D�>�D�~�D�� D�  D�AHD�� D��HD���D�>�D��HD��HD��D�@ D��HD�� D��qD�@ D�� D�D�HD�AHD�� D���D���D�@ D��HD�D�HD�AHD�� D�� D�  D�@ D�� D��HD�  D�>�D�� D�� D���D�@ D�� D�� D�  D�AHD���D��HD�  D�>�D�}qD���D���D�>�D�~�D�� D�  D�@ D��HD�� D���D�B�D�~�D���D�  D�@ D��HD�D�HD�@ D�� D�� D�  D�AHD���D��HD�  D�>�D�� D�� D���D�@ D�}qD��qD�HD�AHD�� D���D���D�>�D�~�D���D���D�@ D�� D���D���D�AHD��HD�� D���D�>�D D�� D��D�>�DÀ D��HD�  D�AHDāHD�� D�  D�>�DŁHD�� D���D�@ Dƀ D�� D�HD�AHD�~�DǽqD�  D�B�DȂ�D�� D��qD�=qD�~�Dɾ�D�  D�@ DʁHD�� D��qD�=qD�~�D˾�D���D�=qD�~�D̾�D���D�<)D�}qD;�D���D�>�D�~�Dξ�D���D�@ D�~�D��HD��D�@ DЁHD�� D���D�@ DсHD�� D�HD�@ D�~�DҾ�D�HD�>�DӀ D��HD�  D�>�DԀ DԾ�D�  D�>�DՀ D�� D�HD�@ Dր D�D�HD�@ DׁHD׾�D�HD�@ D�}qD�� D�  D�>�Dـ D�� D���D�@ DځHD�� D��D�>�Dۀ D���D�  D�@ D܁HDܾ�D�HD�=qD݀ DݽqD�  D�AHD�~�D޾�D�  D�AHD߀ D�� D��qD�@ D�~�DྸD���D�@ DႏD�� D�HD�@ D�|)D�� D���D�AHD� D�D�  D�>�D� D�� D�  D�C�D� D��HD�HD�<)D悏D�D��D�>�D�HD�� D�  D�AHD�HD�D�  D�AHD�HD��HD�HD�AHD� D��HD�HD�AHD�HD�� D�  D�AHD삏D��HD�HD�@ D� D�� D�  D�AHD�HD�� D�  D�B�D� DﾸD��qD�>�D�� D�� D���D�@ D� D��HD���D�>�D�~�D��HD�  D�=qD�~�D�� D�  D�AHD� D���D�HD�AHD�� D��HD�  D�>�D�}qD��HD�  D�@ D�}qD�� D�  D�>�D�� D�� D���D�>�D�~�D��HD�  D�B�D�� D��?�?aG�?�=q?�Q�?�G�?��H@
=@(��@E�@Y��@n{@��
@���@��R@���@�Q�@��@�33@�  @�\)@�(�Az�A(�A33A��A   A%A.{A4z�A:=qA@��AG
=AMp�ATz�A[�Aa�Ah��Ap  Ax��A�Q�A��
A��A�33A��RA��\A�ffA���A�p�A���A�z�A�  A��A�\)A�33A�\)A��
A��A��
AǮA�33A�ffAҏ\A�ffA�=qA�ffA�=qA�RA�\A�RA�=qA�ffA���A�(�B   BB�Bp�B\)B	�B
�\B  Bp�B
=B��B=qB�B�BffB�
B�B�\B�B��B=qB�B z�B!��B"�\B#�B$��B%B&�HB'�
B(��B)B*�HB+�B,��B-��B.�\B/\)B0Q�B1p�B2=qB2�HB3�
B4��B5�B6�HB7�
B8��B9B:�RB;�B<Q�B=p�B>=qB?33B@(�BA�BB{BC\)BD(�BEG�BFffBG�BHQ�BIG�BJ=qBK33BL(�BL��BM�BN�RBO�
BP��BQp�BRffBS33BT  BT��BU��BV�RBW�BXQ�BYp�BZ{B[
=B\  B\��B]B^�\B_�
Ba�Bb{Bc
=Bd  Be�Bf=qBg33Bh  Bh��BiBj�RBk�
Bl��Bm��Bn�HBp(�Bq�Br{Bs
=Bt(�Bu�Bv{Bv�RBw�Bxz�Byp�Bz=qB{33B|(�B}�B~=qB�B�(�B��RB�33B��B�  B�ffB���B�p�B��B�ffB���B��B�(�B��RB�33B���B�  B��\B�
=B���B�(�B��HB�\)B��B�ffB��HB�\)B��
B�ffB���B��B�=qB��RB�G�B��B�(�B��RB�33B��
B�z�B�
=B���B�=qB��RB��B���B�(�B���B�\)B�  B��\B���B�\)B�  B�z�B��B�B�ffB���B�p�B�{B�z�B�
=B��B�{B���B�\)B�  B�z�B��HB�\)B�  B��RB�G�B��
B�Q�B��RB�\)B��
B�ffB��B��
B�ffB���B�G�B��B�ffB�33B��
B�ffB�
=B�p�B��B�z�B�33B��B��\B�
=B���B�(�B��HB���B�Q�B��RB�33B�B�z�B�33B��B��\B�
=B��B�(�B��RB�p�B�(�B���B�\)B�B�ffB�
=B��
B�z�B��HBŅB�{B���BǅB�=qBȣ�B�33BɮBʏ\B�33B��
B�Q�B��HB�p�B�=qB���Bϙ�B�(�B��HB�G�B��
B�Q�B�33B��B�z�B�
=BՅB�(�B���Bי�B�Q�B���B�G�B��Bڣ�B�p�B�{B܏\B��B�Bޏ\B�G�B��B��\B�
=BᙚB�=qB�
=B�B�z�B���B�B�(�B���B�B�=qB�RB�\)B�{B��HB�B�=qB��B�G�B��
B�RB�\)B�  B�z�B��B��B�\B��B�B�=qB���B��
B�z�B���B��B�=qB���B��B�ffB���B��B�Q�B�
=B���B�(�B���B��C (�C z�C C�C�C�
C{CffC��C(�C�C�RC
=Cp�C��C�C\)C��C�Cz�CC
=CQ�C��C(�CQ�C��C	�C	p�C	�C	��C
p�C
��C  CQ�CC�C\)C��C��CffC�RC�C33C��C�C(�C�C�C=qCz�C��C=qC�\C��C(�C�\C�
C�Cz�C�HC�Cp�C�HC=qCp�CC33C�\CC�C�C��C{CffC�HC(�Cp�C�RC33Cz�CC{C�CC{C�\C�
C(�C��C�C33C�C  CG�C��C 
=C ffC ��C!{C!z�C!�C"
=C"z�C"�RC#
=C#�C#�
C${C$p�C$�C%(�C%z�C&  C&Q�C&��C'�C'p�C'�C(33C(�C(��C)G�C)�\C)��C*ffC*��C+{C+z�C+C,{C,�\C,�HC-Q�C-�C-��C.\)C.��C/
=C/p�C/�HC0(�C0�\C1  C1G�C1C2�C2p�C2�HC3Q�C3��C4{C4z�C4��C5=qC5��C5�C6ffC6C7{C7��C7�
C8G�C8C9
=C9z�C9�HC:(�C:�C:��C;Q�C;��C<�C<p�C=  C=G�C=��C>�C>p�C>C?G�C?�\C@  C@p�C@�RCA33CA��CA�CB\)CBCC
=CC�CC��CD(�CD�RCE  CEz�CE��CF(�CF�CG  CGz�CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aͺ^AͼjAͼjAͼjA;wA;wA;wA;wA���A���A���A;wA�A�A;wAͬAͧ�A͡�A͡�A͡�A͡�Aͣ�Aͣ�Aͣ�Aͣ�Aͣ�A͝�Aͣ�Aͥ�Aͧ�A͟�A͝�A͓uA�G�A���A̍PA�bNA�hsAʩ�A���A�5?A���A�XA�"�A�{Aǉ7AǺ^A��/A���AǼjA�bA��/AƧ�AƅA�r�A�bNA�^5A�VA���A�ȴAŕ�A�C�AĮAę�AĴ9A���AčPA���A�O�A�33A�x�A��;A���A�|�A��TA��uA�Q�A��
A��^A��PA�ĜA�E�A�ƨA���A�%A���A��\A��9A�O�A�M�A�z�A�M�A�E�A��A�O�A�`BA��RA��A�M�A�jA�33A���A�ĜA�%A�^5A�
=A��A��RA�ȴA���A�oA���A���A�`BA���A�I�A�ƨA}�wA{�Ax=qAv1'At�RAr�Ao�An�+Am;dAh^5Ae�AcS�A^v�A[x�AZ��AX  AS�AQp�ANĜAK��AI`BAH�DAG��AE��ADbAA�FA@�A?�
A=��A=A;�mA;hsA:A�A8�HA7�^A4�+A/t�A-�mA-K�A,��A,v�A,bA+
=A%l�A#��A"bA!33A�An�A�^A��A�uA��Ap�A"�A�AȴAA��A�/A�`A��A�TA�^A��A �A�;AbA
bA9XA��A�AA�AE�A�A�PA33A1AdZA��A�\AZA �A  A�^A ��A ~�A v�A ffA 5?A 9X@��!@�A�@���@�o@��R@���@���@��@�l�@�ff@��#@��^@�`B@��`@�+@@�~�@�G�@��@�+@�+@��H@�^@蛦@��
@�l�@�dZ@�+@�+@��@�hs@��/@�A�@��@���@��#@���@��@߅@�ȴ@ݲ-@ݩ�@�V@ܴ9@�(�@��@��@�p�@�O�@��@��@؛�@�r�@�b@�S�@�ȴ@�$�@�`B@���@ԋD@���@�Ĝ@ԣ�@�j@�9X@��@Ӯ@�dZ@��y@ҟ�@�{@���@�O�@�&�@��`@��;@�C�@�"�@�@��y@θR@Η�@Η�@Ο�@�^5@�@��@��#@͑h@���@�Q�@��m@˅@��@ʇ+@�@ɲ-@ɡ�@��@��/@��`@���@��/@�Ĝ@�1'@�
=@Ƨ�@�ff@�5?@��@��/@�b@Ý�@Å@�l�@�"�@��@��-@���@��h@�hs@�%@���@�9X@��;@��w@���@���@�(�@�Z@��@�z�@�r�@�;d@���@���@��!@�v�@�{@���@� �@�  @�|�@�o@�ȴ@��\@�=q@�X@��@��D@�(�@�S�@��@��\@�E�@��@��#@��h@�&�@��@�(�@��F@�l�@�33@���@��@��h@�7L@��@�Ĝ@�A�@�(�@��m@�l�@�@��R@�^5@��@���@��h@�hs@��@���@���@���@��@�b@��w@�K�@���@�v�@�V@�=q@��T@��h@�7L@��@���@�z�@��@��
@��F@�t�@�33@�+@�@��y@���@���@���@�5?@���@��7@��7@��@�V@�Z@��@��
@���@�t�@�S�@�C�@�33@�33@�"�@��H@��!@���@�M�@���@���@�I�@�  @���@�K�@�
=@�ff@�=q@�J@��@��T@��h@�7L@��`@���@���@��j@��u@�Q�@�1@��;@���@�dZ@�\)@�;d@�"�@���@��@���@�V@�J@���@��-@�7L@��@�z�@�j@�j@�j@�j@�A�@� �@� �@�b@�b@���@�ƨ@�t�@�dZ@�o@��@���@�hs@�/@���@��`@��9@�j@�Z@�1'@��@�ƨ@�l�@���@�^5@�5?@��@��h@�X@��@���@���@�bN@�A�@��F@�|�@�dZ@�;d@��H@���@�V@�$�@���@�X@���@���@���@��@�z�@�r�@�I�@�ƨ@�S�@��H@���@�n�@�-@��@��^@�?}@���@���@��9@���@�z�@�Q�@�I�@�9X@�b@��@���@��P@�S�@��@���@�~�@�=q@��@��^@���@�x�@�G�@���@��u@�b@���@��P@��H@��\@�V@�J@��#@��h@�`B@�G�@�?}@�7L@�&�@���@��j@���@�bN@�(�@�1@�@l�@~�y@~V@}�T@}�h@}V@|�j@|(�@{��@{o@z^5@z�@y�#@yhs@xĜ@xb@w��@v��@v��@vv�@vE�@u@t�j@r�@rM�@rJ@q�^@qhs@q7L@p��@p��@pb@o|�@o;d@n��@nv�@n5?@l��@l9X@kƨ@j�@j�H@jn�@i��@ix�@iX@h�u@g;d@f��@fE�@f@e�@e`B@eV@d�@dj@dI�@d(�@d1@c�m@c�F@ct�@b��@bn�@a�@a&�@`��@`r�@`Q�@`Q�@`A�@`b@_�@_�@_|�@_+@_
=@_�@_
=@^�R@^E�@]�T@]O�@\�/@\�@\Z@[��@[S�@ZJ@Z�@X��@W��@W�w@XQ�@Xb@W\)@U@T��@T�@T1@S�F@S��@SdZ@SC�@S33@S@R�H@Rn�@Q�^@QG�@P��@P��@P��@P �@O\)@O+@O+@O�@Nȴ@Nff@NE�@M�T@M�h@Mp�@M�@L��@Lz�@K�
@K33@J�!@J�\@Jn�@J^5@J^5@JM�@J-@I��@IX@H�9@H1'@G;d@F��@F��@F�+@Fff@Fff@FE�@E�@D��@D(�@C��@CS�@C33@C"�@C@B��@A��@A�#@A�#@A�^@@�`@@r�@@A�@@ �@?�;@?l�@?+@?
=@>�R@>v�@>v�@>$�@=�T@=�h@=��@=��@=�-@=p�@=O�@<�@<�D@;��@;@:�\@:M�@9x�@9�@8Ĝ@8�`@81'@81'@8A�@8 �@7�@7l�@6��@6V@5��@5��@5?}@4��@4z�@49X@3ƨ@3dZ@3C�@2�@2��@2^5@2J@1�@1��@1�^@1��@0��@0Ĝ@0��@0�@0bN@0 �@0 �@0b@0b@0b@/��@/|�@/;d@.�R@.E�@.@-��@-p�@-`B@-`B@-?}@,�@,�@,9X@,1@,1@+�m@+ƨ@+ƨ@+��@+C�@*�H@*�\@)�#@)��@)�7@)�7@)x�@)�7@)x�@(��@(�u@(�@(bN@(Q�@( �@'�@'l�@';d@'+@&�y@&�R@&v�@%@%O�@%/@$�@$I�@$1@#t�@#dZ@#C�@#"�@"�@"��@"��@"n�@"M�@!��@!�7@!&�@ �`@ �9@ bN@ A�@ 1'@   @��@�w@\)@��@�R@�R@��@��@��@��@�+@ff@��@/@�@�j@��@�D@�D@z�@j@(�@1@�F@��@33@n�@-@�@�@�@�@�@�@�@�@��@�7@G�@�@�@G�@��@1'@b@�@�w@�P@K�@ȴ@v�@V@$�@$�@@O�@�@�@��@I�@(�@�F@�@t�@t�@t�@dZ@33@o@@�H@�!@n�@��@��@��@�^@�^@�^@x�@��@��@�9@�@Q�@ �@  @�w@��@l�@\)@K�@�@
=@�@
=@�y@�@ȴ@v�@@��@��@p�@?}@/@��@�@�/@�j@��@�D@I�@1@�m@ƨ@�@S�@33@@
�H@
�H@
��@
��@
��@
�!@
�!@
��@
�\@
n�@
=q@
�@
J@	��A͸RAͺ^A͸RAͺ^A͸RA;wA;wA;wA;wAͼjA;wAͼjAͼjA���A���A���A�A���A�A�A�A���A���A���A���A;wA;wAͼjAͼjA;wA���A;wA;wA���A�ĜA�ƨA�ƨA�ƨA�ĜA���A���AͼjAͼjA;wAͼjAͺ^Aʹ9Aͧ�Aͧ�AͰ!AͬAͲ-Aͧ�Aͧ�Aͣ�A͟�A͡�A͟�A͟�A͟�A͡�Aͣ�Aͣ�Aͣ�Aͣ�Aͣ�A͡�A͡�A͡�A͟�A͟�Aͣ�Aͥ�Aͥ�Aͥ�Aͥ�Aͥ�A͡�A͟�A͡�A͡�Aͥ�Aͥ�Aͥ�Aͧ�Aͧ�Aͥ�Aͥ�Aͥ�Aͥ�Aͣ�Aͥ�Aͣ�Aͣ�Aͣ�Aͣ�Aͣ�A͡�Aͣ�Aͣ�A͙�A͙�A͙�A͗�A͙�A͙�A͛�A͛�A͙�A͙�A͟�Aͥ�A͡�Aͥ�Aͧ�Aͥ�Aͧ�Aͧ�Aͥ�Aͥ�Aͣ�Aͥ�Aͥ�Aͧ�Aͩ�Aͩ�Aͩ�Aͩ�AͬAͧ�Aͩ�Aͩ�Aͧ�A͟�A͝�A͟�A͡�A͙�A͙�A͙�A͛�A͙�A͛�A͙�A͙�A͛�A͝�A͝�A͝�A͝�A͛�A͝�A͙�A͝�A͙�A͇+A͏\A�n�A�z�A�x�A�z�A�ffA�`BA�/A�VA�A�A�A���A��
A̼jA̰!A̴9A̩�A̙�A̕�Ȁ\A̍PA̋DẢ7Ả7Ȧ+Ȧ+Ȧ+A̅A�|�A�p�A�^5A�O�A�5?A�A��HA�Aˇ+A�bNA�S�A�7LA�(�A�"�A�A��A��yA��A���Aʝ�A�x�A�?}A�7LA��A�%A�A��A��`A�ƨAɺ^Aɰ!Aɣ�A�bNA�A��A��HA��A�  A�1A�
=A�
=A�A��A��
A�ĜA�Aȣ�A�t�A�M�A��A�
=A�
=A�JA��A�(�A�9XA��A��A��A�5?A�C�A�M�A�Q�A�&�A���A��/AǺ^AǙ�AǏ\A�z�A�~�A�|�AǇ+AǛ�Aǝ�Aǟ�Aǥ�AǮAǴ9A���A��mA��yA��A��yA��#A��A���A��
A���A��
A���A���A���A���A���A���A��HA���A���AǺ^Aǟ�AǏ\A�p�A�1'A�oA�JA�%A���A���A��A��yA��mA��;A���A�ȴA���A�ƨAƴ9Aƣ�Aƙ�Aƕ�AƑhAƑhAƍPAƇ+AƃAƁA�|�A�z�A�x�A�r�A�n�A�r�A�t�A�r�A�jA�ffA�`BA�\)A�\)A�`BA�`BA�\)A�\)A�dZA�bNA�\)A�\)A�G�A�7LA�-A��A���A��
A�ƨAžwA�A��/A��;A��;A��A���A���A�ȴA�ƨA�ĜA�ƨA���AŲ-Aţ�AœuAŉ7AŁA�|�A�r�A�dZA�VA�33A�JA�A�  A��HAčPAāA�~�Aď\Aĕ�Aĉ7AđhAę�Aĥ�Aħ�Aİ!AĸRAĸRAĴ9Aħ�AĶFAļjAĲ-AĲ-AľwA�ƨA���A���A�AĲ-Aħ�Aė�AăA�33A�A�n�A�1A§�A�\)A�"�A���A���A�|�A�S�A�%A��A�K�A�?}A�5?A�(�A��A��A�%A��mA���A�bNA�&�A���A�l�A��A��A���A���A��uA�p�A�7LA�  A��DA�/A���A��A���A��PA���A�dZA�+A��A�%A��yA��A���A�A���A��
A���A�x�A�t�A�jA�hsA�VA�VA�VA�VA�M�A�A�A�M�A�=qA��`A��;A���A�E�A���A�%A���A��7A�`BA�A��mA���A���A�v�A�bNA�(�A�VA���A��mA��jA���A�?}A���A�/A��A��A��uA���A�33A�&�A��/A��hA�~�A�\)A��A���A��hA��+A�K�A�5?A�&�A�{A�A���A��yA��#A��/A���A�ȴA���A��wA��9A���A���A��uA�|�A�hsA�C�A��A��7A�A�A�1A���A���A��A�dZA�+A�%A��mA��wA�ffA��A���A��wA��!A���A��DA�ffA�33A�%A��A���A�5?A��hA�v�A�`BA�Q�A�E�A�7LA�&�A��A�JA���A��mA���A��A�&�A�p�A��A���A�ffA��RA�z�A�^5A�5?A���A�ZA��A���A��A��HA�ĜA��!A��A���A���A���A��PA�\)A�/A��A�%A��TA���A��!A��A�G�A�?}A�=qA�33A�/A�$�A�oA��A��+A�ZA�I�A�33A��A���A��DA� �A��`A���A�Q�A���A�bNA�ȴA��A���A�z�A��;A��;A��\A�G�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                             Aͺ^AͼjAͼjAͼjA;wA;wA;wA;wA���A���A���A;wA�A�A;wAͬAͧ�A͡�A͡�A͡�A͡�Aͣ�Aͣ�Aͣ�Aͣ�Aͣ�A͝�Aͣ�Aͥ�Aͧ�A͟�A͝�A͓uA�G�A���A̍PA�bNA�hsAʩ�A���A�5?A���A�XA�"�A�{Aǉ7AǺ^A��/A���AǼjA�bA��/AƧ�AƅA�r�A�bNA�^5A�VA���A�ȴAŕ�A�C�AĮAę�AĴ9A���AčPA���A�O�A�33A�x�A��;A���A�|�A��TA��uA�Q�A��
A��^A��PA�ĜA�E�A�ƨA���A�%A���A��\A��9A�O�A�M�A�z�A�M�A�E�A��A�O�A�`BA��RA��A�M�A�jA�33A���A�ĜA�%A�^5A�
=A��A��RA�ȴA���A�oA���A���A�`BA���A�I�A�ƨA}�wA{�Ax=qAv1'At�RAr�Ao�An�+Am;dAh^5Ae�AcS�A^v�A[x�AZ��AX  AS�AQp�ANĜAK��AI`BAH�DAG��AE��ADbAA�FA@�A?�
A=��A=A;�mA;hsA:A�A8�HA7�^A4�+A/t�A-�mA-K�A,��A,v�A,bA+
=A%l�A#��A"bA!33A�An�A�^A��A�uA��Ap�A"�A�AȴAA��A�/A�`A��A�TA�^A��A �A�;AbA
bA9XA��A�AA�AE�A�A�PA33A1AdZA��A�\AZA �A  A�^A ��A ~�A v�A ffA 5?A 9X@��!@�A�@���@�o@��R@���@���@��@�l�@�ff@��#@��^@�`B@��`@�+@@�~�@�G�@��@�+@�+@��H@�^@蛦@��
@�l�@�dZ@�+@�+@��@�hs@��/@�A�@��@���@��#@���@��@߅@�ȴ@ݲ-@ݩ�@�V@ܴ9@�(�@��@��@�p�@�O�@��@��@؛�@�r�@�b@�S�@�ȴ@�$�@�`B@���@ԋD@���@�Ĝ@ԣ�@�j@�9X@��@Ӯ@�dZ@��y@ҟ�@�{@���@�O�@�&�@��`@��;@�C�@�"�@�@��y@θR@Η�@Η�@Ο�@�^5@�@��@��#@͑h@���@�Q�@��m@˅@��@ʇ+@�@ɲ-@ɡ�@��@��/@��`@���@��/@�Ĝ@�1'@�
=@Ƨ�@�ff@�5?@��@��/@�b@Ý�@Å@�l�@�"�@��@��-@���@��h@�hs@�%@���@�9X@��;@��w@���@���@�(�@�Z@��@�z�@�r�@�;d@���@���@��!@�v�@�{@���@� �@�  @�|�@�o@�ȴ@��\@�=q@�X@��@��D@�(�@�S�@��@��\@�E�@��@��#@��h@�&�@��@�(�@��F@�l�@�33@���@��@��h@�7L@��@�Ĝ@�A�@�(�@��m@�l�@�@��R@�^5@��@���@��h@�hs@��@���@���@���@��@�b@��w@�K�@���@�v�@�V@�=q@��T@��h@�7L@��@���@�z�@��@��
@��F@�t�@�33@�+@�@��y@���@���@���@�5?@���@��7@��7@��@�V@�Z@��@��
@���@�t�@�S�@�C�@�33@�33@�"�@��H@��!@���@�M�@���@���@�I�@�  @���@�K�@�
=@�ff@�=q@�J@��@��T@��h@�7L@��`@���@���@��j@��u@�Q�@�1@��;@���@�dZ@�\)@�;d@�"�@���@��@���@�V@�J@���@��-@�7L@��@�z�@�j@�j@�j@�j@�A�@� �@� �@�b@�b@���@�ƨ@�t�@�dZ@�o@��@���@�hs@�/@���@��`@��9@�j@�Z@�1'@��@�ƨ@�l�@���@�^5@�5?@��@��h@�X@��@���@���@�bN@�A�@��F@�|�@�dZ@�;d@��H@���@�V@�$�@���@�X@���@���@���@��@�z�@�r�@�I�@�ƨ@�S�@��H@���@�n�@�-@��@��^@�?}@���@���@��9@���@�z�@�Q�@�I�@�9X@�b@��@���@��P@�S�@��@���@�~�@�=q@��@��^@���@�x�@�G�@���@��u@�b@���@��P@��H@��\@�V@�J@��#@��h@�`B@�G�@�?}@�7L@�&�@���@��j@���@�bN@�(�@�1@�@l�@~�y@~V@}�T@}�h@}V@|�j@|(�@{��@{o@z^5@z�@y�#@yhs@xĜ@xb@w��@v��@v��@vv�@vE�@u@t�j@r�@rM�@rJ@q�^@qhs@q7L@p��@p��@pb@o|�@o;d@n��@nv�@n5?@l��@l9X@kƨ@j�@j�H@jn�@i��@ix�@iX@h�u@g;d@f��@fE�@f@e�@e`B@eV@d�@dj@dI�@d(�@d1@c�m@c�F@ct�@b��@bn�@a�@a&�@`��@`r�@`Q�@`Q�@`A�@`b@_�@_�@_|�@_+@_
=@_�@_
=@^�R@^E�@]�T@]O�@\�/@\�@\Z@[��@[S�@ZJ@Z�@X��@W��@W�w@XQ�@Xb@W\)@U@T��@T�@T1@S�F@S��@SdZ@SC�@S33@S@R�H@Rn�@Q�^@QG�@P��@P��@P��@P �@O\)@O+@O+@O�@Nȴ@Nff@NE�@M�T@M�h@Mp�@M�@L��@Lz�@K�
@K33@J�!@J�\@Jn�@J^5@J^5@JM�@J-@I��@IX@H�9@H1'@G;d@F��@F��@F�+@Fff@Fff@FE�@E�@D��@D(�@C��@CS�@C33@C"�@C@B��@A��@A�#@A�#@A�^@@�`@@r�@@A�@@ �@?�;@?l�@?+@?
=@>�R@>v�@>v�@>$�@=�T@=�h@=��@=��@=�-@=p�@=O�@<�@<�D@;��@;@:�\@:M�@9x�@9�@8Ĝ@8�`@81'@81'@8A�@8 �@7�@7l�@6��@6V@5��@5��@5?}@4��@4z�@49X@3ƨ@3dZ@3C�@2�@2��@2^5@2J@1�@1��@1�^@1��@0��@0Ĝ@0��@0�@0bN@0 �@0 �@0b@0b@0b@/��@/|�@/;d@.�R@.E�@.@-��@-p�@-`B@-`B@-?}@,�@,�@,9X@,1@,1@+�m@+ƨ@+ƨ@+��@+C�@*�H@*�\@)�#@)��@)�7@)�7@)x�@)�7@)x�@(��@(�u@(�@(bN@(Q�@( �@'�@'l�@';d@'+@&�y@&�R@&v�@%@%O�@%/@$�@$I�@$1@#t�@#dZ@#C�@#"�@"�@"��@"��@"n�@"M�@!��@!�7@!&�@ �`@ �9@ bN@ A�@ 1'@   @��@�w@\)@��@�R@�R@��@��@��@��@�+@ff@��@/@�@�j@��@�D@�D@z�@j@(�@1@�F@��@33@n�@-@�@�@�@�@�@�@�@�@��@�7@G�@�@�@G�@��@1'@b@�@�w@�P@K�@ȴ@v�@V@$�@$�@@O�@�@�@��@I�@(�@�F@�@t�@t�@t�@dZ@33@o@@�H@�!@n�@��@��@��@�^@�^@�^@x�@��@��@�9@�@Q�@ �@  @�w@��@l�@\)@K�@�@
=@�@
=@�y@�@ȴ@v�@@��@��@p�@?}@/@��@�@�/@�j@��@�D@I�@1@�m@ƨ@�@S�@33@@
�H@
�H@
��@
��@
��@
�!@
�!@
��@
�\@
n�@
=q@
�@
J@	��A͸RAͺ^A͸RAͺ^A͸RA;wA;wA;wA;wAͼjA;wAͼjAͼjA���A���A���A�A���A�A�A�A���A���A���A���A;wA;wAͼjAͼjA;wA���A;wA;wA���A�ĜA�ƨA�ƨA�ƨA�ĜA���A���AͼjAͼjA;wAͼjAͺ^Aʹ9Aͧ�Aͧ�AͰ!AͬAͲ-Aͧ�Aͧ�Aͣ�A͟�A͡�A͟�A͟�A͟�A͡�Aͣ�Aͣ�Aͣ�Aͣ�Aͣ�A͡�A͡�A͡�A͟�A͟�Aͣ�Aͥ�Aͥ�Aͥ�Aͥ�Aͥ�A͡�A͟�A͡�A͡�Aͥ�Aͥ�Aͥ�Aͧ�Aͧ�Aͥ�Aͥ�Aͥ�Aͥ�Aͣ�Aͥ�Aͣ�Aͣ�Aͣ�Aͣ�Aͣ�A͡�Aͣ�Aͣ�A͙�A͙�A͙�A͗�A͙�A͙�A͛�A͛�A͙�A͙�A͟�Aͥ�A͡�Aͥ�Aͧ�Aͥ�Aͧ�Aͧ�Aͥ�Aͥ�Aͣ�Aͥ�Aͥ�Aͧ�Aͩ�Aͩ�Aͩ�Aͩ�AͬAͧ�Aͩ�Aͩ�Aͧ�A͟�A͝�A͟�A͡�A͙�A͙�A͙�A͛�A͙�A͛�A͙�A͙�A͛�A͝�A͝�A͝�A͝�A͛�A͝�A͙�A͝�A͙�A͇+A͏\A�n�A�z�A�x�A�z�A�ffA�`BA�/A�VA�A�A�A���A��
A̼jA̰!A̴9A̩�A̙�A̕�Ȁ\A̍PA̋DẢ7Ả7Ȧ+Ȧ+Ȧ+A̅A�|�A�p�A�^5A�O�A�5?A�A��HA�Aˇ+A�bNA�S�A�7LA�(�A�"�A�A��A��yA��A���Aʝ�A�x�A�?}A�7LA��A�%A�A��A��`A�ƨAɺ^Aɰ!Aɣ�A�bNA�A��A��HA��A�  A�1A�
=A�
=A�A��A��
A�ĜA�Aȣ�A�t�A�M�A��A�
=A�
=A�JA��A�(�A�9XA��A��A��A�5?A�C�A�M�A�Q�A�&�A���A��/AǺ^AǙ�AǏ\A�z�A�~�A�|�AǇ+AǛ�Aǝ�Aǟ�Aǥ�AǮAǴ9A���A��mA��yA��A��yA��#A��A���A��
A���A��
A���A���A���A���A���A���A��HA���A���AǺ^Aǟ�AǏ\A�p�A�1'A�oA�JA�%A���A���A��A��yA��mA��;A���A�ȴA���A�ƨAƴ9Aƣ�Aƙ�Aƕ�AƑhAƑhAƍPAƇ+AƃAƁA�|�A�z�A�x�A�r�A�n�A�r�A�t�A�r�A�jA�ffA�`BA�\)A�\)A�`BA�`BA�\)A�\)A�dZA�bNA�\)A�\)A�G�A�7LA�-A��A���A��
A�ƨAžwA�A��/A��;A��;A��A���A���A�ȴA�ƨA�ĜA�ƨA���AŲ-Aţ�AœuAŉ7AŁA�|�A�r�A�dZA�VA�33A�JA�A�  A��HAčPAāA�~�Aď\Aĕ�Aĉ7AđhAę�Aĥ�Aħ�Aİ!AĸRAĸRAĴ9Aħ�AĶFAļjAĲ-AĲ-AľwA�ƨA���A���A�AĲ-Aħ�Aė�AăA�33A�A�n�A�1A§�A�\)A�"�A���A���A�|�A�S�A�%A��A�K�A�?}A�5?A�(�A��A��A�%A��mA���A�bNA�&�A���A�l�A��A��A���A���A��uA�p�A�7LA�  A��DA�/A���A��A���A��PA���A�dZA�+A��A�%A��yA��A���A�A���A��
A���A�x�A�t�A�jA�hsA�VA�VA�VA�VA�M�A�A�A�M�A�=qA��`A��;A���A�E�A���A�%A���A��7A�`BA�A��mA���A���A�v�A�bNA�(�A�VA���A��mA��jA���A�?}A���A�/A��A��A��uA���A�33A�&�A��/A��hA�~�A�\)A��A���A��hA��+A�K�A�5?A�&�A�{A�A���A��yA��#A��/A���A�ȴA���A��wA��9A���A���A��uA�|�A�hsA�C�A��A��7A�A�A�1A���A���A��A�dZA�+A�%A��mA��wA�ffA��A���A��wA��!A���A��DA�ffA�33A�%A��A���A�5?A��hA�v�A�`BA�Q�A�E�A�7LA�&�A��A�JA���A��mA���A��A�&�A�p�A��A���A�ffA��RA�z�A�^5A�5?A���A�ZA��A���A��A��HA�ĜA��!A��A���A���A���A��PA�\)A�/A��A�%A��TA���A��!A��A�G�A�?}A�=qA�33A�/A�$�A�oA��A��+A�ZA�I�A�33A��A���A��DA� �A��`A���A�Q�A���A�bNA�ȴA��A���A�z�A��;A��;A��\A�G�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
l�B
l�B
lWB
lWB
k�B
l"B
l"B
lWB
l"B
l�B
k�B
l"B
k�B
l"B
kB
m)B
m�B
o�B
pB
poB
p�B
p;B
poB
p�B
qB
p�B
rB
qAB
qB
p�B
rGB
rGB
s�B
}�B
��B
��B
�7B
��B
��B
�}B
�?B�B
�fB
��B�B�BhB%zB)�B7LB33B?�BI�BGEBH�BPHBX�Bg8Bs�B�eB�CB�B��B��B�WB�B�BMB�B�B�B�B�xB�8B�jB�/B��B�&BBAB �B	�B��B@B�WB� B�|B��B��B;B�`B��B�B�B�HB��B�B��B�B�Bo5Bk�B_�BJ#B*�B iB
��B
��B
�'B
|PB
j�B
Y�B
N�B
?}B
6FB
0�B
&LB
1B
xB	�5B	�BB	�)B	�B	��B	�UB	��B	�:B	��B	�7B	p�B	`BB	X�B	V�B	LdB	?�B	=B	2�B	.�B	'�B	'B	%�B	'�B	*�B	+kB	33B	A�B	A�B	E�B	DgB	H�B	H�B	K�B	RTB	aHB	R�B	RTB	MjB	MjB	I�B	H�B	2�B	'RB	~B	YB	�B	(B	�B	@B	YB	B	!�B	'�B	'�B	&�B	'�B	 \B	�B	#:B	�B	 'B	!�B	"hB	%zB	,qB	#B	B	{B	4B		7B	PB	�B		B	IB	7B	�B	�B	�B	bB	FB	�B	�B	1B	VB	"4B	#�B	&�B	(�B	0�B	6zB	/�B	.�B	.�B	-�B	.}B	/�B	0UB	7�B	5�B	5�B	4nB	4�B	6B	5B	8RB	4�B	9�B	<jB	:�B	:�B	<B	?}B	A�B	C-B	B�B	B�B	C�B	F�B	I�B	J#B	LdB	OvB	PHB	T�B	Y�B	bNB	e,B	d&B	gB	e�B	f�B	h�B	f�B	h�B	qB	sB	rGB	rB	rGB	rB	sB	r�B	s�B	u�B	w2B	{B	|�B	.B	�uB	��B	�\B	��B	�B	�oB	�B	�B	�$B	��B	�YB	��B	�B	��B	��B	�B	�_B	��B	�kB	��B	��B	��B	��B	��B	�\B	�-B	��B	�bB	�4B	�4B	��B	�RB	��B	��B	�_B	��B	��B	�B	�}B	�IB	�IB	��B	�B	�UB	��B	�9B	�B	��B	�hB	�hB	�B	�tB	�B	��B	��B	��B	�B	�*B	��B	�^B	��B	�B	��B	ÖB	��B	�EB	ȀB	��B	�B	ϫB	�B	��B	�mB	�WB	�/B	�]B	��B	��B	�TB	��B	��B	�yB	�B	�B	�B	��B	��B	��B	�cB	�/B	��B	�B	�AB	�B	�vB	�B	��B	�B	�B	��B	��B	�+B	�+B	��B	��B	�B	�>B	��B	�B	��B	�JB	�JB	��B	�B	�VB	��B	��B	�.B
 �B
 �B
�B
�B
MB
�B
YB
�B
�B
	lB

	B
xB
B
�B
B
�B
VB
VB
�B
�B
bB
bB
hB
oB
oB
uB
uB
@B
uB
@B
�B
uB
B
�B
{B
{B
FB
FB
�B
�B
FB
�B
FB
FB
FB
B
�B
B
B
FB
�B
uB
@B
@B
uB
:B
B
�B
oB
�B
�B
B
B
B
�B
�B
B
B
�B
�B
�B
B
�B
$B
YB
�B
_B
+B
_B
�B
�B
�B
�B
�B
kB
�B
�B
�B
IB
IB
~B
~B
�B
B
�B
�B
�B
�B
�B
VB
�B
 \B
 \B
 \B
 'B
�B
�B
B
�B
�B
�B
OB
B
�B
!B
 'B
!bB
 �B
!B
�B
VB
�B
VB
�B
�B
 \B
 \B
 \B
!�B
!bB
!-B
!�B
"4B
"hB
"�B
"�B
#B
$tB
$�B
$�B
%FB
%FB
%B
$�B
%FB
&LB
&�B
'�B
'�B
($B
(�B
(�B
)�B
+B
+6B
+kB
+�B
+�B
,B
,B
+�B
+�B
,�B
,=B
,�B
-CB
-CB
-�B
/B
.IB
.�B
/B
.�B
.�B
.�B
.}B
/OB
/B
0UB
0!B
0�B
1�B
1�B
2aB
2�B
2�B
33B
2�B
33B
33B
3hB
33B
3�B
3�B
3�B
49B
49B
49B
4nB
4nB
4�B
4�B
5B
4�B
5tB
6B
6B
6�B
6zB
7�B
7�B
7�B
8B
8�B
8�B
9$B
9�B
9XB
9�B
9$B
8�B
9�B
;0B
:�B
:�B
;0B
;dB
;0B
;dB
;�B
<jB
=<B
=�B
>wB
>�B
>�B
?�B
?�B
?�B
@OB
@�B
A�B
AUB
@�B
A B
B'B
A�B
B�B
B�B
B�B
CaB
B�B
C-B
CaB
C-B
CaB
C�B
CaB
C�B
C�B
C�B
FB
G�B
IB
J�B
J�B
K^B
K�B
K�B
K�B
K�B
K^B
J�B
K�B
K^B
J�B
K�B
K)B
K�B
K�B
L0B
L�B
K�B
K�B
K�B
K�B
K�B
I�B
J�B
K�B
L0B
K�B
NB
N�B
N�B
MjB
L�B
MB
M6B
M�B
MB
MjB
M�B
NB
NB
OB
P�B
Q�B
R B
S�B
S�B
S&B
S[B
S�B
T�B
T�B
T�B
T�B
U�B
U2B
UgB
T�B
U2B
U�B
V9B
VmB
W?B
W?B
W�B
XB
XEB
XB
W�B
WsB
WsB
W�B
XEB
XB
YB
ZB
ZQB
Y�B
YB
ZB
YB
YKB
ZB
Z�B
[#B
[WB
[�B
\)B
[�B
[�B
\)B
\]B
[�B
\]B
[�B
]dB
]�B
]�B
^5B
^�B
^jB
_;B
_B
_B
^5B
^�B
_B
_pB
^�B
^jB
^�B
_B
`�B
`�B
aHB
`�B
a�B
aB
`�B
`�B
a�B
bNB
bNB
c�B
d�B
d�B
ffB
e�B
ffB
f2B
f�B
g�B
hsB
g�B
g�B
hsB
hsB
hsB
iDB
iyB
iyB
i�B
iyB
i�B
jB
jB
i�B
iDB
iyB
i�B
jKB
jB
jB
jB
jKB
jB
jB
i�B
i�B
jB
jKB
j�B
kQB
k�B
lWB
l"B
lWB
k�B
k�B
k�B
l"B
l"B
l�B
m]B
m�B
m�B
m]B
l�B
l�B
l�B
l�B
l�B
ncB
n�B
n�B
n/B
n�B
o B
oiB
p�B
qB
qAB
qvB
q�B
q�B
q�B
q�B
qvB
qvB
rB
r�B
rB
sB
s�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
uZB
uZB
uZB
u�B
u�B
u%B
uZB
u�B
t�B
u�B
uZB
uZB
u�B
u�B
v+B
u�B
wfB
w�B
w�B
xlB
w�B
x8B
w�B
w�B
w�B
w�B
w2B
x8B
x�B
yrB
yrB
y�B
yrB
y�B
y�B
yrB
z�B
z�B
{�B
{JB
|PB
|PB
|�B
}"B
|�B
|PB
|�B
|�B
|�B
|�B
|�B
{�B
{�B
{B
|�B
|�B
~]B
}�B
}"B
|�B
}VB
}�B
}�B
}�B
}�B
}�B
}�B
~]B
~]B
�B
cB
�B
~�B
� B
� B
�B
��B
��B
�B
�oB
�B
��B
�oB
��B
��B
��B
�AB
��B
��B
��B
��B
��B
�GB
��B
��B
��B
�MB
��B
�{B
��B
��B
��B
��B
�B
��B
��B
��B
��B
�SB
��B
��B
��B
��B
�SB
�%B
��B
�YB
�%B
�YB
��B
��B
�+B
��B
�_B
�_B
�_B
�_B
�1B
��B
��B
�B
�lB
�lB
��B
��B
��B
�lB
�7B
�lB
��B
�	B
�	B
�lB
��B
�=B
��B
�rB
�=B
��B
l�B
m�B
m�B
m)B
m�B
k�B
lWB
k�B
k�B
l"B
k�B
lWB
k�B
kQB
k�B
kB
kB
kQB
kB
kQB
kQB
k�B
l"B
k�B
l�B
l�B
m]B
m]B
l�B
lWB
k�B
k�B
k�B
k�B
i�B
jB
j�B
kB
k�B
l�B
k�B
m]B
k�B
k�B
l�B
l"B
m�B
n/B
m�B
k�B
l�B
j�B
m]B
m�B
oiB
p�B
poB
p�B
qB
p�B
p;B
oiB
oiB
oiB
o�B
o�B
p;B
p;B
qAB
qvB
q�B
pB
o�B
o�B
oiB
o�B
pB
qB
qvB
qvB
qvB
p;B
o�B
pB
o�B
o�B
pB
o�B
pB
pB
p�B
pB
p�B
qB
qAB
qvB
qB
rB
qvB
qvB
t�B
sB
sMB
s�B
r�B
sB
sB
r�B
sB
r�B
q�B
p�B
r|B
qAB
qB
qAB
poB
p�B
qB
q�B
qB
qAB
poB
o�B
oiB
pB
o�B
pB
oiB
p�B
oiB
p�B
poB
rB
sMB
rB
r|B
tB
sMB
s�B
s�B
s�B
sMB
s�B
s�B
sB
r|B
r�B
r|B
r|B
r�B
rGB
q�B
q�B
rGB
x8B
rGB
z�B
uZB
v`B
t�B
y>B
y>B
�_B
��B
��B
�%B
��B
�1B
�VB
�4B
�4B
�JB
�B
��B
�uB
�MB
��B
��B
�B
��B
��B
�SB
�B
��B
��B
��B
��B
��B
�OB
��B
��B
�FB
��B
��B
��B
�IB
��B
�UB
��B
�LB
�zB
��B
��B
�3B
�0B
�pB
�pB
�TB
уB
��B
�B
�B
ʌB
͟B
�B
�XB
��B
�mB
�?B
ܒB
��B
�B
��B
��B iB	B
rBSB
�.B
��B%B  B
��B
��B
�WB
�B
�B
�iB
�B
�]B�B �B{B"B�BMB�B�BbB�B�B�B_BB�B%BSB�B�B�BB�B�B"BB�B$B%zB'RB'�B%FB&LB%�B&�B'�B+kB,qB*eB+6B)�B-B*�BF�B<B=<B6B:�B7LB0�B0UB2�B4nB3�B5B8�B@�BCaBFtBC�BCaBL0BK^BJXBIRBHKBG�BGzBH�BFBGzBGEBG�BF�BD�BE9BF�BIBN<BM�BM�BN<BQBQ�BQNBQBR BR�BU�BWsBYKB]�BZBd�Bd�Bc�Bf2Bk�BkQBc�BcTBd�Bq�Bz�B~�B��B�MB�$B��B�_B��B�B��B�LB��B��B�[B�B�B��B��B��B�qB�LB��B��B�dB�B�XB�B�jB��B��B��B�XB��B�B��B�B��B�jB�/BںB�B�TB�2B�B��B��B�AB�`B��B�B�fB��B�B(B+B$B+BB�BIB7B�BB"4B 'B7B_B�B�B1BYB�B�BBYBB�BhB	�B{B �B;B��B��B��B�DB;B��B�B�B�B��B��B�5B��B�DB��B��B��B�/B�/B�mB�B�BݘB�|BܒB�WB�pB��B�#B�B�]B�B�B�#BیB�)B�B� B��BB �B�PB�BB  B�.BoBAB�cB�B;B��B  B�B�VB�B
rB�BB�B�VB$@B�B�B�JB�ZB�B�GB�B 'B9�B�TB� B��B��B��B�cB��B�WB��B�WB��B�B�B��B�GB�B�B�B�B��B�B�B�B�/B�TB�B��B�B�VBoB�VB��BuB
rB�]B�(B�>B��B�2B��B�>B��B� B�B�B��B �B�B��B�B��B�AB�B�B��B��B��B�B��B�B�BB�#B��BΥB՛B�hB�B��B��B��B��B��B��B�-B�DB�B��B�	B�=B�1B��B��B�B�MB��B��B��B��B�eB�4BpoBncBqBm�Bl�Bn/Bo BsBc�B_;B_;B_B]/Bf�B]dBN�BO�BI�BHBM�BB'B:�B!�B$@B-B�B�B;G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                             B
e B
d�B
dcB
dcB
c�B
d.B
d.B
dcB
d.B
d�B
c�B
d.B
c�B
d.B
c(B
e5B
fB
g�B
hB
h{B
h�B
hGB
h{B
h�B
iB
h�B
jB
iMB
iB
h�B
jSB
jSB
k�B
u�B
��B
��B
�CB
��B
��B
ȉB
�KB
��B
�rB
��B	�B
��B	tB�B!�B/XB+?B7�BA�B?QB@�BHTBP�B_DBk�B�qB�OB�)B��B��B�cB�B�BYB�B�B�B��B�B�DB�vB�;B�B�2B�%B�MB��B�B�BLB�cB�B�B��B� B�GB�lB�B�B�B�TB��B�'B��B�B�BgABc�BW�BB/B"�B
�uB
��B
��B
�3B
t\B
b�B
Q�B
F�B
7�B
.RB
(�B
XB
=B
�B	�AB	�NB	�5B	�#B	��B	�aB	��B	�FB	��B	�CB	h�B	XNB	P�B	N�B	DpB	7�B	5B	*�B	&�B	�B	*B	�B	�B	"�B	#wB	+?B	9�B	9�B	=�B	<sB	@�B	@�B	C�B	J`B	YTB	J�B	J`B	EvB	EvB	A�B	@�B	+B	^B	�B	eB		�B	4B	B	LB	eB	B	B	�B	�B	�B	�B	hB	�B	FB	�B	3B	�B	tB	�B	$}B	B	'B	�B		@B	CB	\B	�B	B	UB	CB	�B	�B	�B	nB	RB	�B	�B	=B	bB	@B	�B	�B	 �B	(�B	.�B	'�B	&�B	&�B	%�B	&�B	'�B	(aB	/�B	-�B	-�B	,zB	,�B	.B	-B	0^B	,�B	2B	4vB	3B	2�B	4B	7�B	9�B	;9B	;B	:�B	;�B	>�B	A�B	B/B	DpB	G�B	HTB	L�B	Q�B	ZZB	]8B	\2B	_B	^
B	^�B	`�B	^�B	`�B	iB	k%B	jSB	jB	jSB	jB	k%B	j�B	k�B	m�B	o>B	s"B	t�B	w:B	z�B	��B	�hB	��B	�B	�{B	�B	�*B	�0B	��B	�eB	�B	�B	��B	��B	�B	�kB	��B	�wB	��B	��B	��B	��B	��B	�hB	�9B	�B	�nB	�@B	�@B	��B	�^B	��B	��B	�kB	��B	��B	�B	��B	�UB	�UB	��B	�'B	�aB	�B	�EB	�B	��B	�tB	�tB	�B	��B	�B	��B	��B	��B	�B	�6B	�B	�jB	��B	�B	��B	��B	�
B	�QB	��B	��B	�B	ǷB	�&B	��B	�yB	�cB	�;B	�iB	��B	��B	�`B	�B	��B	�B	�B	�B	��B	��B	��B	��B	�oB	�;B	��B	�B	�MB	�B	�B	�B	��B	�B	�B	��B	�B	�7B	�7B	��B	��B	�B	�JB	��B	�B	��B	�VB	�VB	�B	�(B	�bB	��B	� B	�:B	��B	��B	��B	��B	�YB	��B	�eB	��B
 �B
xB
B
�B
!B
�B
'B
�B
bB
bB
�B
�B
nB
nB
	tB

{B

{B
�B
�B
LB
�B
LB
�B
�B
B
�B
�B
�B
RB
RB
�B
�B
RB
�B
RB
RB
RB
B
�B
B
B
RB
�B
�B
LB
LB
�B

FB

B
	�B

{B

�B

�B
B
$B
$B
�B
�B
*B
*B
�B
�B
�B
*B
�B
0B
eB
B
kB
7B
kB
�B
�B
�B
�B
�B
wB
�B
�B
�B
UB
UB
�B
�B
�B
'B
�B
�B
�B
�B
�B
bB
�B
hB
hB
hB
3B
�B
�B
'B
�B
�B
�B
[B
'B
�B
-B
3B
nB
�B
-B
�B
bB
�B
bB
�B
�B
hB
hB
hB
�B
nB
9B
�B
@B
tB
�B
�B
B
�B
�B
�B
RB
RB
B
�B
RB
XB
�B
�B
�B
 0B
 �B
!B
!�B
#B
#BB
#wB
#�B
#�B
$B
$B
#�B
#�B
$�B
$IB
$�B
%OB
%OB
%�B
''B
&UB
&�B
''B
&�B
&�B
&�B
&�B
'[B
''B
(aB
(-B
(�B
*B
*B
*mB
*�B
*�B
+?B
+B
+?B
+?B
+tB
+?B
+�B
+�B
+�B
,EB
,EB
,EB
,zB
,zB
,�B
,�B
-B
,�B
-�B
.B
.B
.�B
.�B
/�B
/�B
/�B
0)B
0�B
0�B
10B
1�B
1dB
1�B
10B
0�B
1�B
3<B
3B
3B
3<B
3pB
3<B
3pB
3�B
4vB
5HB
5�B
6�B
6�B
6�B
7�B
7�B
7�B
8[B
8�B
9�B
9aB
8�B
9,B
:3B
9�B
:�B
:�B
:�B
;mB
:�B
;9B
;mB
;9B
;mB
;�B
;mB
;�B
;�B
<
B
>B
?�B
A)B
CB
CB
CjB
C�B
C�B
C�B
C�B
CjB
B�B
C�B
CjB
CB
C�B
C5B
DB
C�B
D<B
D�B
C�B
DB
C�B
C�B
C�B
A�B
B�B
C�B
D<B
DB
FB
F�B
F�B
EvB
D�B
EB
EBB
E�B
EB
EvB
E�B
FB
FB
GB
H�B
I�B
J,B
K�B
LB
K2B
KgB
LB
L�B
M
B
L�B
L�B
M�B
M>B
MsB
M
B
M>B
M�B
NEB
NyB
OKB
OKB
O�B
PB
PQB
PB
O�B
OB
OB
O�B
PQB
PB
Q#B
R)B
R]B
Q�B
Q�B
R)B
Q�B
QWB
R)B
R�B
S/B
ScB
S�B
T5B
S�B
S�B
T5B
TiB
TB
TiB
TB
UpB
U�B
U�B
VAB
V�B
VvB
WGB
WB
WB
VAB
V�B
WB
W|B
V�B
VvB
V�B
WB
X�B
X�B
YTB
X�B
Y�B
YB
X�B
X�B
Y�B
ZZB
ZZB
[�B
]B
]B
^rB
]�B
^rB
^>B
^�B
_�B
`B
_�B
_�B
`B
`B
`B
aPB
a�B
a�B
a�B
a�B
a�B
b"B
b"B
a�B
aPB
a�B
a�B
bWB
b�B
b�B
b�B
bWB
b"B
b"B
a�B
a�B
b�B
bWB
b�B
c]B
c�B
dcB
d.B
dcB
c�B
c�B
c�B
d.B
d.B
e B
eiB
e�B
e�B
eiB
d�B
d�B
e B
d�B
e B
foB
f�B
f�B
f;B
f�B
gB
guB
h�B
iB
iMB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
jB
j�B
jB
k%B
k�B
j�B
k�B
k�B
k�B
k�B
l�B
l�B
mfB
mfB
mfB
m�B
m�B
m1B
mfB
m�B
l�B
m�B
mfB
mfB
m�B
m�B
n7B
nB
orB
o�B
o�B
pxB
o�B
pDB
o�B
o�B
o�B
o�B
o>B
pDB
p�B
q~B
q~B
q�B
q~B
q�B
q�B
q~B
r�B
r�B
s�B
sVB
t\B
t\B
t�B
u.B
t�B
t\B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
s"B
t�B
t�B
viB
u�B
u.B
t�B
ubB
u�B
u�B
u�B
u�B
v B
u�B
viB
viB
w�B
woB
w�B
v�B
xB
xB
w�B
x�B
x�B
yB
y{B
yB
x�B
y{B
y�B
y�B
y�B
zMB
z�B
z�B
z�B
z�B
z�B
{SB
{�B
|�B
|�B
|YB
{�B
{�B
|�B
|�B
|�B
}�B
}+B
}�B
}�B
}�B
}�B
}_B
|�B
|�B
}�B
}�B
}_B
~1B
~�B
~eB
~1B
~eB
~�B
B
7B
�B
kB
kB
kB
kB
�=B
��B
��B
�B
�xB
�xB
��B
��B
��B
�xB
�CB
�xB
��B
�B
�B
�xB
��B
�IB
��B
�~B
�IB
��B
e B
fB
e�B
e5B
e�B
c�B
dcB
c�B
c�B
d.B
c�B
dcB
c�B
c]B
c�B
c(B
c(B
c]B
c(B
c]B
c]B
c�B
d.B
c�B
d�B
e B
eiB
eiB
e B
dcB
c�B
c�B
c�B
c�B
a�B
b�B
b�B
c(B
c�B
d�B
c�B
eiB
c�B
c�B
e B
d.B
fB
f;B
e�B
c�B
d�B
b�B
eiB
e�B
guB
h�B
h{B
h�B
iB
h�B
hGB
guB
guB
guB
g�B
g�B
hGB
hGB
iMB
i�B
i�B
hB
g�B
g�B
guB
g�B
hB
iB
i�B
i�B
i�B
hGB
g�B
hB
g�B
g�B
hB
g�B
hB
hB
h�B
hB
h�B
iB
iMB
i�B
iB
jB
i�B
i�B
l�B
k%B
kYB
k�B
j�B
k%B
k%B
j�B
k%B
j�B
i�B
h�B
j�B
iMB
iB
iMB
h{B
h�B
iB
i�B
iB
iMB
h{B
g�B
guB
hB
g�B
hB
guB
h�B
guB
h�B
h{B
jB
kYB
jB
j�B
l+B
kYB
k�B
k�B
k�B
kYB
k�B
k�B
k%B
j�B
j�B
j�B
j�B
j�B
jSB
i�B
i�B
jSB
pDB
jSB
r�B
mfB
nlB
l�B
qJB
qJB
kB
{�B
}�B
~1B
}�B
�=B
�bB
�@B
�@B
�VB
�B
��B
��B
�YB
��B
��B
�*B
��B
��B
�_B
�*B
��B
�B
��B
��B
��B
�[B
��B
��B
�RB
��B
��B
��B
�UB
��B
�aB
��B
�XB
��B
��B
��B
�?B
�<B
�|B
�|B
�`B
ɏB
��B
�&B
� B
B
ūB
�)B
�dB
��B
�yB
�KB
ԞB
��B
�B
��B
��B
�uBB~B
�_B
�:B
��B
�1B
�B
��B
�B
�cB
�B
�B
�uB
�B
�iB
��B
��B
��B.B	�BYB�B�BnB�B
��B
��B
�kB
�+B
��B
�1B
�_B
��B�B�B'B
�B	�B.B!B�BB�B^B�BRBXB�B�B�B#wB$}B"qB#BB!�B%B"�B>�B4B5HB.B2�B/XB(�B(aB*�B,zB+�B-B0�B8�B;mB>�B;�B;mBD<BCjBBdBA^B@WB?�B?�B@�B>B?�B?QB?�B>�B<�B=EB>�BA)BFHBE�BE�BFHBI&BI�BIZBI&BJ,BJ�BM�BOBQWBU�BR)B\�B\�B[�B^>Bc�Bc]B[�B[`B\�Bi�Br�BwB��B�YB�0B�B�kB��B�B��B�XB��B��B�gB�B�B��B��B��B�}B�XB��B��B�pB�#B�dB�#B�vB��B��B��B�dB��B� B��B�)B��B�vB�;B��B�%B�`B�>B�B��B��B�MB�lB�B�+B�rB��B��B4B7B0B7BB�BUBCB�BB@B3BCBkB�B�B=BeB�B�B!BeBB
�B	tB�B��B��B�GB��B��B��B�PB�GB��B�BܛBݡB��B�B�AB��B�PB��B�B��B�;B�;B�yBыB��BդBوBԞB�cB�|B�B�/B�)B�iB�)B�B�/BӘB�5B�B�B�B*B��B�\B��B�B�B�:B�{B�MB�oB��B�GB��B�B��B�bB��B~B�BB��B�bBLB�B�+B�VB�fB�%B�SB��B3B1�B�`B�B��B��B��B�oB��B�cB��B�cB� B�B�B��B�SB�B�B�B�B��B�B�B�B�;B�`B�"B��B�B�bB�{B�bB�B��B~B�iB�4B�JB�	B�>B�B�JB��B�B�B�B��B��B�B��B��B��B�MB�B�B��B��B��BܛB�B�B�NB�/B��BƱBͧB�tB�B��B��B��B��B��B��B�9B�PB�'B��B�B�IB�=B�B��B|%B|YBBx�B{�B~�B�qBx@Bh{BfoBiBe�Be Bf;BgBk%B[�BWGBWGBWBU;B^�BUpBF�BG�BA�B@#BE�B:3B2�BBLB%B�B
��B
�GG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223241                            20230426223241AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622324120230426223241  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622324120230426223241QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622324120230426223241QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               