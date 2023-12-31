CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-09-25T13:06:18Z creation; 2023-04-26T19:24:34Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.5   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  d|   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � Al   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � h�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �p   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �,   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �D   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20220925130618  20230426192434  5905274 5905274 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315_008643_173                 7315_008643_173                 2C  2C  DD  SOLO_II                         SOLO_II                         8643                            8643                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @��]:��@��]:��11  @��]���@��]���@0�^��#@0�^��#�da"����da"���11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @B�\@}p�@��R@��R@�p�A ��AG�A\)A,��A@��A`  A�  A��A�\)A��A�  AϮA߮A�\)A�\)B�
BQ�B(�B�B'�
B0(�B8(�B@  BH(�BPQ�BX(�B`(�Bh  Bo�
Bw�B�
B�{B�{B��B�  B�{B�  B�{B�  B��B�{B�{B�  B��
B�  B�(�B�  B��B�{B�(�B�  B�  B�{B�{B��B��
B�  B��B�B��
B��B�  C �C{C�C{C  C	��C  C�C�C�C  C��C  C
=C  C��C��C"
=C#��C%��C(  C*  C+�C.  C0
=C2  C4  C6  C8
=C:  C<  C>
=C@  CA�CC��CE�CG��CJ
=CL  CM��CP
=CR
=CT
=CV
=CX  CZ  C\  C^
=C`
=Cb
=Cd
=Ce�Cg��Cj  Cl
=Cn  Cp  Cr  Cs��Cu��Cx  Cy��C|  C~  C�C���C�C�  C���C���C�  C�C�  C�  C�C�C���C�  C���C�  C�  C���C�  C�C�  C�  C�
=C�  C�  C���C���C�
=C�
=C�  C�  C�C���C���C�  C�C�  C���C��C���C���C���C�C�
=C�  C���C���C�C�  C���C�C�
=C�C�  C���C�  C�
=C�
=C�C�C�
=C�C�C�  C���C�  C���C���C�C�C�C�C���C���C���C���C�  C�C�C�  C���C���C���C�  C���C�  C�C�C�  C�  C�  C�C�C�  C�  C�C�  C���C���C���C���C���C�  C�  C�  C�  C�C�
=C�C�  C�  C�  C�  C���C���C���C�C�  C�  C���C��C�  C�C�C�C�C�C���C���D }qD  D� D�D� D�D�D�qDz�D  D��D��Dz�D�D� D  D�D	�D	� D
  D
}qD
�qD}qD  D��D�D}qD  D�D�qD}qD  D�DD}qD�qD��D�D}qD��DxRD�qD� D�qD}qD  D� D�D�D�D��DD��D�D��D�D� D�D��DD�DD�D �D ��D �qD!� D"D"��D"��D#xRD#�qD$��D%�D%�D%�qD&z�D'  D'� D'�qD(�D)D)�D*�D*z�D*��D+� D,D,��D-  D-��D.D.� D.�qD/��D0�D0}qD1  D1��D1�qD2}qD3D3� D3��D4z�D5�D5��D5�qD6� D7D7��D7�qD8��D9
=D9� D9�qD:��D;  D;}qD<�D<��D=  D=� D>D>}qD>�qD?�D@�D@� DA  DA}qDB  DB� DC  DCz�DC��DD}qDD��DE}qDE�qDF}qDG  DG}qDG��DH� DI�DI� DJ�DJ�DK  DK}qDL  DL��DMDM� DM�qDN� DO  DO}qDO��DP}qDP�qDQ}qDR�DR� DR��DS}qDT�DT��DU  DU� DV�DV��DV��DWz�DX  DX� DY  DY� DZ  DZ� D[  D[� D[�qD\}qD]�D]� D]�qD^� D_�D_��D`  D`�Da  Da� Db  Dbz�Db�qDc}qDc��Dd}qDd�qDe��DfDf��Dg  Dg� Dh  Dh}qDi  Di� Di�qDj}qDk  Dk� Dl�Dl�Dm�Dm}qDn�Dn��Dn�qDo}qDp  Dp��Dq�Dq��Dq�qDr� Ds�Ds� Dt  DtxRDt�RDuz�Du�qDvz�Dv�qDw}qDx  Dx�DyDy��DzDz��D{  D{� D|  D|}qD}�D}�D~�D~}qD  D� D�qD�=qD�~�D�� D�HD�@ D���D�D�  D�>�D�~�D���D�HD�B�D�� D���D�  D�AHD�� D���D���D�=qD�� D�D�  D�=qD��HD�� D��qD�>�D�}qD�� D��D�@ D��HD�� D���D�=qD�� D�D�HD�B�D�~�D�� D���D�@ D�� D���D�  D�@ D��HD�� D��qD�>�D�� D�� D�HD�@ D�}qD���D���D�@ D��HD�� D��qD�>�D��HD�� D���D�AHD�� D���D�HD�>�D�~�D�� D�  D�>�D��HD��HD�  D�@ D��HD���D���D�@ D�� D��HD�  D�=qD�~�D�� D���D�>�D�� D��qD�  D�AHD��HD�� D���D�>�D�~�D���D��qD�@ D�~�D��qD�  D�=qD�~�D��qD���D�B�D��HD�� D���D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�� D�� D�HD�AHD��HD��HD�HD�B�D�~�D�� D��D�B�D�� D��qD���D�>�D�~�D�� D�  D�=qD�~�D���D���D�@ D��HD���D��qD�>�D�}qD�� D�  D�=qD�}qD�� D�  D�>�D�~�D���D�  D�B�D���D�D��D�AHD�~�D���D�HD�@ D�� D��HD��D�@ D�� D�D��D�>�D�~�D���D�  D�@ D�� D��HD�  D�@ D�� D���D�  D�AHD���D���D��qD�>�D�~�D���D�HD�B�D�� D���D�  D�AHD�� D��qD�  D�AHD�� D���D���D�=qD�|)D��)D��qD�=qD�~�D�D��D�B�D�� D��HD��D�B�D��HD�� D�HD�@ D�~�D�� D���D�=qD�~�D�� D�  D�>�D�}qD��qD��qD�<)D�}qD��qD��)D�<)D�~�D�� D�  D�>�D�� D��HD�  D�AHD��HD���D�  D�B�D�� D��qD�  D�B�D�� D�� D�  D�>�DHD��HD�  D�=qDÀ D�� D�  D�@ D�}qD�� D�HD�@ D�~�D��HD�HD�>�D�~�D�� D�  D�@ Dǀ D�D�HD�@ DȁHD��HD�  D�@ DɁHD��HD�HD�>�D�~�D��HD�HD�@ Dˀ D˾�D�  D�AHD�~�D�� D�  D�@ D�~�D;�D�  D�B�D΂�D��HD��D�AHDπ DϾ�D���D�>�DЁHDо�D��qD�=qD�~�D��HD�HD�@ DҁHD�D��D�AHDӁHD�D��D�@ DԀ D�� D�  D�AHDՀ Dվ�D���D�>�Dր D��HD�  D�>�D׀ D׾�D��qD�>�D�~�Dؾ�D��qD�@ DفHD��HD�HD�AHDځHD�� D���D�>�D�~�D۽qD���D�@ D܂�D�D�HD�@ D�~�Dݾ�D���D�=qD�~�D�� D�HD�>�D�}qD߽qD�  D�AHD�~�D��HD��D�=qD�}qD�� D�  D�@ D�HD�D��D�@ D� D��HD���D�AHD䂏D��HD�  D�@ D�}qD�� D��qD�AHD� D澸D��D�AHD�HD羸D��D�@ D�}qD�� D���D�C�D�HD�� D��qD�>�D�~�D�D���D�@ D�~�D뾸D�HD�>�D삏D쾸D���D�>�D� D�� D���D�>�D� DD��qD�AHD�~�D��HD���D�@ D�~�D�D��D�AHD� D�� D�HD�AHD� D��HD�HD�AHD�~�D��D���D�B�D� D���D�  D�>�D�~�D���D��qD�AHD�� D��HD�HD�>�D�~�D�� D�  D�=qD�~�D�� D���D�>�D��HD���D�  D�=qD��HD��D���?��?#�
?W
=?�\)?��
?\?�(�?�@��@�R@.{@=p�@Tz�@fff@xQ�@��
@���@��@���@��
@�\)@���@�G�@�=q@��@�(�@�G�@���@��@��HA�A
=A��A��A�A��A(�A ��A$z�A*�HA/\)A3�
A7�A9��A>�RAB�\AG�AL(�AP��ATz�AX��A\��A`��Adz�Ag
=Aj=qAn�RAs33AvffAz�HA�Q�A�=qA��
A�p�A��A���A��HA���A�
=A�G�A�33A�{A�Q�A�=qA���A��RA���A��\A��
A�{A���A��\A��A�\)A���A�33A���A�
=A�G�A��A�ffA���A\A�z�AƸRAȣ�Aʏ\A��
A�A�  A��A�(�A�{A�Q�A�=qA�z�A�
=A�G�A�33A�p�A�
=A�Q�A�\A�z�A�A�A�A��A�ffA���A��HA��A�\)B�B{B33B  B��B�B�HB  B��B
=qB�Bz�B��BffB33BQ�Bp�BffB�B��B{B
=B(�B�B=qB33BQ�BG�B=qB33B   B ��B!�B#33B$(�B%G�B&=qB'\)B(Q�B)p�B*�\B+�
B-�B.{B/
=B0  B1�B2{B2�RB3�
B4��B5�B6�HB8  B9�B:ffB;�B<��B=G�B>=qB?33B@Q�BAp�BBffBC�BD��BEBG33BH(�BI�BJ=qBK33BL  BL��BN{BO
=BP(�BQ�BR�RBS�BT��BU�BV�RBW�BXQ�BY��BZ�RB[�B\��B]�B_\)B`Q�Bap�Bb=qBc33Bd  Be�Bf=qBg\)BhQ�Bip�Bj�\Bl  Bl��Bn{Bo
=Bp(�Bp��BqBr�HBtQ�Bup�BvffBw\)Bx(�Bx��Bz{B{33B|(�B}G�B~�HB�B�ffB���B�G�B��B�(�B��RB�G�B��
B�Q�B��HB�\)B��B�z�B��B���B�=qB��RB�33B�B�(�B���B��B��B�{B���B��B��B�{B��RB�33B��B�Q�B���B�p�B��B�Q�B��HB�\)B�B�(�B���B�33B��B�=qB���B�\)B��B�ffB��HB�\)B�B�(�B���B��B���B�(�B��\B�33B�B�Q�B���B�G�B��
B�{B��\B��B��B�(�B���B�33B��B�ffB���B��B�  B��\B�
=B���B�{B�z�B���B�\)B��
B�Q�B���B�G�B��B�ffB��HB�\)B��B�ffB��B��B�(�B���B��B��B�(�B���B�33B��B��B�z�B�
=B���B�{B���B��B���B�(�B���B�p�B��B�ffB��HB�\)B��B�Q�B���B�33B��B�  B�z�B�
=B���B�(�B��RB�33B���B�(�B\B�
=B�p�B��
B�Q�Bģ�B�33BŮB�(�Bƣ�B�G�BǮB�=qBȸRB�33B�B�ffB���B�\)B�B�=qB���B�\)B�B�ffB���BυB�  BЏ\B�
=BѮB�=qBҸRB�G�B��
B�ffB��HBՅB�  B֏\B�
=BׅB�(�B؏\B�
=BمB�(�Bڣ�B�G�B��
B�Q�B��HB�\)B��
B�ffB��HB�\)B��B�ffB���B�p�B�  B�z�B�
=B㙚B�{B��B��B�B�=qB���B�p�B�  B�z�B��B陚B�(�B�RB�G�B�B�ffB���B�B�  B��B��B�B�=qB���B�p�B�{B�\B�33B�B�ffB���B�p�B�  B���B�33B��
B�ffB�
=B���B�=qB���B��B�{B��RB�\)B��B���B�G�B��C =qC �\C �C=qC�\C�
C(�C\)C�C��CG�C�\C��CQ�C��C��CQ�C��C�HC33C�\C�
C=qC��C��CQ�C��C�C	33C	�C	��C
(�C
z�C
�
C�C�\C��CG�C��C�C33C�\C�HC33C��C  CQ�C�C��C33Cz�C�
C{CffC�RC  C=qCp�C��CC�HC��C�C33CQ�C\)C�C�\CC�HC��C{C�C33CG�CQ�Cp�C�C��CC�C  C(�C33CG�CG�Cp�Cz�C��C��C�C
=C�C=qCQ�Cp�Cp�C�C��CC��C��C(�C=qC\)Cp�C�\C��C��CC�HC��C
=C(�C\)Cz�C�\C�RCC��C�HC  C�CQ�CffC�C�C��CC�
C��C{CG�CffCz�C�C��C�RCC  C�C=qC\)C\)C�C��CC�C
=C33CG�CQ�Cp�C�\C��C�HC
=C(�CG�CG�Cp�C�\C�C�HC
=C(�CG�Cz�C�C��C�RC�
C  C=qC\)C�C��C��C�
C�C �C Q�C p�C ��C ��C ��C �C!(�C!Q�C!z�C!�\C!��C!��C!�C"�C"\)C"z�C"��C"�C"�
C"��C#�C#ffC#�C#�RC#C#�HC$
=C$=qC$p�C$��C$C$��C%  C%{C%\)C%��C%�RC%�C%��C&(�C&G�C&z�C&�C&�HC'{C'=qC'Q�C'z�C'�C'�C({C(G�C(Q�C(�C(C(��C)�C)33C)\)C)��C)�
C*  C*33C*G�C*ffC*��C*�HC+
=C+=qC+G�C+z�C+�RC+��C,(�C,33C,ffC,�\C,C-
=C-33C-\)C-z�C-��C-�
C.�C.Q�C.�C.��C.��C.��C/(�C/z�C/��C/�RC/�C0{C0ffC0��C0C0�
C1  C1=qC1�C1�RC1�
C1��C2(�C2p�C2��C2�RC2�
C3�C3Q�C3�C3��C3C3�C4=qC4\)C4�C4��C4C5
=C5=qC5ffC5z�C5�C5�
C6�C6G�C6\)C6�C6�RC6�C7�C7(�C7\)C7��C7C7��C8  C833C8\)C8��C8C8�
C9  C9G�C9p�C9�C9��C9��C:�C:(�C:Q�C:�C:C:�
C;  C;33C;p�C;��C;�RC;��C;��C<=qC<ffC<�C<��C<�
C={C==qC=G�C=p�C=�RC=�C=�C>�C>Q�C>�C>�\C>�C>��C?�C?33C?Q�C?��C?�RC?��C?��C@33C@\)C@ffC@�\C@�
C@��CA
=CA33CAp�CA�CA��CA�HCB{CB�CBG�CB\)CB��CB�RCB�
CC{CCG�CCG�CCp�CC�CC�
CC�HCC��CDG�CDffCDp�CD��CD��CD�HCE  CE(�CE\)CE�CE��CECF  CF(�CF33CFQ�CF��CFCF�
CG
=CG=qCGQ�CGp�CG��CG�
CG�CH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                               ?�  @   @B�\@}p�@��R@��R@�p�A ��AG�A\)A,��A@��A`  A�  A��A�\)A��A�  AϮA߮A�\)A�\)B�
BQ�B(�B�B'�
B0(�B8(�B@  BH(�BPQ�BX(�B`(�Bh  Bo�
Bw�B�
B�{B�{B��B�  B�{B�  B�{B�  B��B�{B�{B�  B��
B�  B�(�B�  B��B�{B�(�B�  B�  B�{B�{B��B��
B�  B��B�B��
B��B�  C �C{C�C{C  C	��C  C�C�C�C  C��C  C
=C  C��C��C"
=C#��C%��C(  C*  C+�C.  C0
=C2  C4  C6  C8
=C:  C<  C>
=C@  CA�CC��CE�CG��CJ
=CL  CM��CP
=CR
=CT
=CV
=CX  CZ  C\  C^
=C`
=Cb
=Cd
=Ce�Cg��Cj  Cl
=Cn  Cp  Cr  Cs��Cu��Cx  Cy��C|  C~  C�C���C�C�  C���C���C�  C�C�  C�  C�C�C���C�  C���C�  C�  C���C�  C�C�  C�  C�
=C�  C�  C���C���C�
=C�
=C�  C�  C�C���C���C�  C�C�  C���C��C���C���C���C�C�
=C�  C���C���C�C�  C���C�C�
=C�C�  C���C�  C�
=C�
=C�C�C�
=C�C�C�  C���C�  C���C���C�C�C�C�C���C���C���C���C�  C�C�C�  C���C���C���C�  C���C�  C�C�C�  C�  C�  C�C�C�  C�  C�C�  C���C���C���C���C���C�  C�  C�  C�  C�C�
=C�C�  C�  C�  C�  C���C���C���C�C�  C�  C���C��C�  C�C�C�C�C�C���C���D }qD  D� D�D� D�D�D�qDz�D  D��D��Dz�D�D� D  D�D	�D	� D
  D
}qD
�qD}qD  D��D�D}qD  D�D�qD}qD  D�DD}qD�qD��D�D}qD��DxRD�qD� D�qD}qD  D� D�D�D�D��DD��D�D��D�D� D�D��DD�DD�D �D ��D �qD!� D"D"��D"��D#xRD#�qD$��D%�D%�D%�qD&z�D'  D'� D'�qD(�D)D)�D*�D*z�D*��D+� D,D,��D-  D-��D.D.� D.�qD/��D0�D0}qD1  D1��D1�qD2}qD3D3� D3��D4z�D5�D5��D5�qD6� D7D7��D7�qD8��D9
=D9� D9�qD:��D;  D;}qD<�D<��D=  D=� D>D>}qD>�qD?�D@�D@� DA  DA}qDB  DB� DC  DCz�DC��DD}qDD��DE}qDE�qDF}qDG  DG}qDG��DH� DI�DI� DJ�DJ�DK  DK}qDL  DL��DMDM� DM�qDN� DO  DO}qDO��DP}qDP�qDQ}qDR�DR� DR��DS}qDT�DT��DU  DU� DV�DV��DV��DWz�DX  DX� DY  DY� DZ  DZ� D[  D[� D[�qD\}qD]�D]� D]�qD^� D_�D_��D`  D`�Da  Da� Db  Dbz�Db�qDc}qDc��Dd}qDd�qDe��DfDf��Dg  Dg� Dh  Dh}qDi  Di� Di�qDj}qDk  Dk� Dl�Dl�Dm�Dm}qDn�Dn��Dn�qDo}qDp  Dp��Dq�Dq��Dq�qDr� Ds�Ds� Dt  DtxRDt�RDuz�Du�qDvz�Dv�qDw}qDx  Dx�DyDy��DzDz��D{  D{� D|  D|}qD}�D}�D~�D~}qD  D� D�qD�=qD�~�D�� D�HD�@ D���D�D�  D�>�D�~�D���D�HD�B�D�� D���D�  D�AHD�� D���D���D�=qD�� D�D�  D�=qD��HD�� D��qD�>�D�}qD�� D��D�@ D��HD�� D���D�=qD�� D�D�HD�B�D�~�D�� D���D�@ D�� D���D�  D�@ D��HD�� D��qD�>�D�� D�� D�HD�@ D�}qD���D���D�@ D��HD�� D��qD�>�D��HD�� D���D�AHD�� D���D�HD�>�D�~�D�� D�  D�>�D��HD��HD�  D�@ D��HD���D���D�@ D�� D��HD�  D�=qD�~�D�� D���D�>�D�� D��qD�  D�AHD��HD�� D���D�>�D�~�D���D��qD�@ D�~�D��qD�  D�=qD�~�D��qD���D�B�D��HD�� D���D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�� D�� D�HD�AHD��HD��HD�HD�B�D�~�D�� D��D�B�D�� D��qD���D�>�D�~�D�� D�  D�=qD�~�D���D���D�@ D��HD���D��qD�>�D�}qD�� D�  D�=qD�}qD�� D�  D�>�D�~�D���D�  D�B�D���D�D��D�AHD�~�D���D�HD�@ D�� D��HD��D�@ D�� D�D��D�>�D�~�D���D�  D�@ D�� D��HD�  D�@ D�� D���D�  D�AHD���D���D��qD�>�D�~�D���D�HD�B�D�� D���D�  D�AHD�� D��qD�  D�AHD�� D���D���D�=qD�|)D��)D��qD�=qD�~�D�D��D�B�D�� D��HD��D�B�D��HD�� D�HD�@ D�~�D�� D���D�=qD�~�D�� D�  D�>�D�}qD��qD��qD�<)D�}qD��qD��)D�<)D�~�D�� D�  D�>�D�� D��HD�  D�AHD��HD���D�  D�B�D�� D��qD�  D�B�D�� D�� D�  D�>�DHD��HD�  D�=qDÀ D�� D�  D�@ D�}qD�� D�HD�@ D�~�D��HD�HD�>�D�~�D�� D�  D�@ Dǀ D�D�HD�@ DȁHD��HD�  D�@ DɁHD��HD�HD�>�D�~�D��HD�HD�@ Dˀ D˾�D�  D�AHD�~�D�� D�  D�@ D�~�D;�D�  D�B�D΂�D��HD��D�AHDπ DϾ�D���D�>�DЁHDо�D��qD�=qD�~�D��HD�HD�@ DҁHD�D��D�AHDӁHD�D��D�@ DԀ D�� D�  D�AHDՀ Dվ�D���D�>�Dր D��HD�  D�>�D׀ D׾�D��qD�>�D�~�Dؾ�D��qD�@ DفHD��HD�HD�AHDځHD�� D���D�>�D�~�D۽qD���D�@ D܂�D�D�HD�@ D�~�Dݾ�D���D�=qD�~�D�� D�HD�>�D�}qD߽qD�  D�AHD�~�D��HD��D�=qD�}qD�� D�  D�@ D�HD�D��D�@ D� D��HD���D�AHD䂏D��HD�  D�@ D�}qD�� D��qD�AHD� D澸D��D�AHD�HD羸D��D�@ D�}qD�� D���D�C�D�HD�� D��qD�>�D�~�D�D���D�@ D�~�D뾸D�HD�>�D삏D쾸D���D�>�D� D�� D���D�>�D� DD��qD�AHD�~�D��HD���D�@ D�~�D�D��D�AHD� D�� D�HD�AHD� D��HD�HD�AHD�~�D��D���D�B�D� D���D�  D�>�D�~�D���D��qD�AHD�� D��HD�HD�>�D�~�D�� D�  D�=qD�~�D�� D���D�>�D��HD���D�  D�=qD��HD��G�O�?��?#�
?W
=?�\)?��
?\?�(�?�@��@�R@.{@=p�@Tz�@fff@xQ�@��
@���@��@���@��
@�\)@���@�G�@�=q@��@�(�@�G�@���@��@��HA�A
=A��A��A�A��A(�A ��A$z�A*�HA/\)A3�
A7�A9��A>�RAB�\AG�AL(�AP��ATz�AX��A\��A`��Adz�Ag
=Aj=qAn�RAs33AvffAz�HA�Q�A�=qA��
A�p�A��A���A��HA���A�
=A�G�A�33A�{A�Q�A�=qA���A��RA���A��\A��
A�{A���A��\A��A�\)A���A�33A���A�
=A�G�A��A�ffA���A\A�z�AƸRAȣ�Aʏ\A��
A�A�  A��A�(�A�{A�Q�A�=qA�z�A�
=A�G�A�33A�p�A�
=A�Q�A�\A�z�A�A�A�A��A�ffA���A��HA��A�\)B�B{B33B  B��B�B�HB  B��B
=qB�Bz�B��BffB33BQ�Bp�BffB�B��B{B
=B(�B�B=qB33BQ�BG�B=qB33B   B ��B!�B#33B$(�B%G�B&=qB'\)B(Q�B)p�B*�\B+�
B-�B.{B/
=B0  B1�B2{B2�RB3�
B4��B5�B6�HB8  B9�B:ffB;�B<��B=G�B>=qB?33B@Q�BAp�BBffBC�BD��BEBG33BH(�BI�BJ=qBK33BL  BL��BN{BO
=BP(�BQ�BR�RBS�BT��BU�BV�RBW�BXQ�BY��BZ�RB[�B\��B]�B_\)B`Q�Bap�Bb=qBc33Bd  Be�Bf=qBg\)BhQ�Bip�Bj�\Bl  Bl��Bn{Bo
=Bp(�Bp��BqBr�HBtQ�Bup�BvffBw\)Bx(�Bx��Bz{B{33B|(�B}G�B~�HB�B�ffB���B�G�B��B�(�B��RB�G�B��
B�Q�B��HB�\)B��B�z�B��B���B�=qB��RB�33B�B�(�B���B��B��B�{B���B��B��B�{B��RB�33B��B�Q�B���B�p�B��B�Q�B��HB�\)B�B�(�B���B�33B��B�=qB���B�\)B��B�ffB��HB�\)B�B�(�B���B��B���B�(�B��\B�33B�B�Q�B���B�G�B��
B�{B��\B��B��B�(�B���B�33B��B�ffB���B��B�  B��\B�
=B���B�{B�z�B���B�\)B��
B�Q�B���B�G�B��B�ffB��HB�\)B��B�ffB��B��B�(�B���B��B��B�(�B���B�33B��B��B�z�B�
=B���B�{B���B��B���B�(�B���B�p�B��B�ffB��HB�\)B��B�Q�B���B�33B��B�  B�z�B�
=B���B�(�B��RB�33B���B�(�B\B�
=B�p�B��
B�Q�Bģ�B�33BŮB�(�Bƣ�B�G�BǮB�=qBȸRB�33B�B�ffB���B�\)B�B�=qB���B�\)B�B�ffB���BυB�  BЏ\B�
=BѮB�=qBҸRB�G�B��
B�ffB��HBՅB�  B֏\B�
=BׅB�(�B؏\B�
=BمB�(�Bڣ�B�G�B��
B�Q�B��HB�\)B��
B�ffB��HB�\)B��B�ffB���B�p�B�  B�z�B�
=B㙚B�{B��B��B�B�=qB���B�p�B�  B�z�B��B陚B�(�B�RB�G�B�B�ffB���B�B�  B��B��B�B�=qB���B�p�B�{B�\B�33B�B�ffB���B�p�B�  B���B�33B��
B�ffB�
=B���B�=qB���B��B�{B��RB�\)B��B���B�G�B��C =qC �\C �C=qC�\C�
C(�C\)C�C��CG�C�\C��CQ�C��C��CQ�C��C�HC33C�\C�
C=qC��C��CQ�C��C�C	33C	�C	��C
(�C
z�C
�
C�C�\C��CG�C��C�C33C�\C�HC33C��C  CQ�C�C��C33Cz�C�
C{CffC�RC  C=qCp�C��CC�HC��C�C33CQ�C\)C�C�\CC�HC��C{C�C33CG�CQ�Cp�C�C��CC�C  C(�C33CG�CG�Cp�Cz�C��C��C�C
=C�C=qCQ�Cp�Cp�C�C��CC��C��C(�C=qC\)Cp�C�\C��C��CC�HC��C
=C(�C\)Cz�C�\C�RCC��C�HC  C�CQ�CffC�C�C��CC�
C��C{CG�CffCz�C�C��C�RCC  C�C=qC\)C\)C�C��CC�C
=C33CG�CQ�Cp�C�\C��C�HC
=C(�CG�CG�Cp�C�\C�C�HC
=C(�CG�Cz�C�C��C�RC�
C  C=qC\)C�C��C��C�
C�C �C Q�C p�C ��C ��C ��C �C!(�C!Q�C!z�C!�\C!��C!��C!�C"�C"\)C"z�C"��C"�C"�
C"��C#�C#ffC#�C#�RC#C#�HC$
=C$=qC$p�C$��C$C$��C%  C%{C%\)C%��C%�RC%�C%��C&(�C&G�C&z�C&�C&�HC'{C'=qC'Q�C'z�C'�C'�C({C(G�C(Q�C(�C(C(��C)�C)33C)\)C)��C)�
C*  C*33C*G�C*ffC*��C*�HC+
=C+=qC+G�C+z�C+�RC+��C,(�C,33C,ffC,�\C,C-
=C-33C-\)C-z�C-��C-�
C.�C.Q�C.�C.��C.��C.��C/(�C/z�C/��C/�RC/�C0{C0ffC0��C0C0�
C1  C1=qC1�C1�RC1�
C1��C2(�C2p�C2��C2�RC2�
C3�C3Q�C3�C3��C3C3�C4=qC4\)C4�C4��C4C5
=C5=qC5ffC5z�C5�C5�
C6�C6G�C6\)C6�C6�RC6�C7�C7(�C7\)C7��C7C7��C8  C833C8\)C8��C8C8�
C9  C9G�C9p�C9�C9��C9��C:�C:(�C:Q�C:�C:C:�
C;  C;33C;p�C;��C;�RC;��C;��C<=qC<ffC<�C<��C<�
C={C==qC=G�C=p�C=�RC=�C=�C>�C>Q�C>�C>�\C>�C>��C?�C?33C?Q�C?��C?�RC?��C?��C@33C@\)C@ffC@�\C@�
C@��CA
=CA33CAp�CA�CA��CA�HCB{CB�CBG�CB\)CB��CB�RCB�
CC{CCG�CCG�CCp�CC�CC�
CC�HCC��CDG�CDffCDp�CD��CD��CD�HCE  CE(�CE\)CE�CE��CECF  CF(�CF33CFQ�CF��CFCF�
CG
=CG=qCGQ�CGp�CG��CG�
CG�CH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                               @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�"@�JG�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�$�A�$�A�$�A� �A� �A� �A�"�A�$�A�(�A�(�A�&�A�bA���Aݺ^A�9XA���A��TAܼjAܛ�A�~�A�XA�I�A�=qA�33A�(�A�"�A��A��A��A�{A�{A�VA�1A�1A�1A�A�A���A���A���A���A��A��;A۲-AۅA�oA�oA�1A�1A��A�=qA�{A���A�bA��`A��Aԕ�A�M�A�G�A���A�E�A�M�A���AͲ-A��HA��A���AÙ�A��A���A�z�A�Q�A��A�+A��FA��HA�|�A���A�1A���A�$�A��hA�
=A�ffA�jA���A��A�VA��A��FA�oA��mA��PA�|�A�&�A��TA���A��PA�p�A�G�A�%A��A� �A��\A���A���A�Q�A�;dA�|�A� �A�r�A���A���A��A�ĜA~Q�A|�RA{\)AxI�As��Apr�Aj�/AgAbQ�A_+AZ�AVr�AT��AQ�;AP{AO/AN�AJ��AF��AD^5ACS�AB  A?��A=�A;|�A9�7A7`BA4ĜA2M�A1�wA1p�A0�A/�-A,ĜA+hsA*5?A)�hA)G�A(5?A'�wA'�^A'��A';dA&�A%�A%�A%O�A$A#%A!hsA ��A JA?}A�A�-AhsAC�A�9A^5A�A��AbNA�;A��AS�A�`A  A`BAoAȴAVA��A��A7LA�AA|�A�Ax�A+A%A�9A9XA1A�mA�A��A��A��A�A�!AI�AJA��A�AS�A�/AQ�AJA�A��A��A?}A
�yA
 �AĜA-AG�A+A�A�jA��A��A�uA�+An�A=qA�mAC�A�HA^5A�FA&�A�wA ~�A bNA M�A 9XA J@��@�V@���@���@��@��R@��#@���@��7@�@��u@�1'@��@�@��@�w@�p�@��@���@�9@�I�@�"�@�E�@��@�A�@�b@��H@�^5@���@���@畁@�!@���@�x�@�G�@�x�@��@�x�@�G�@�?}@��`@�z�@�9X@�"�@�n�@���@�X@�&�@�%@��D@��@�"�@ݡ�@ۥ�@��@�J@���@�A�@�  @�|�@�
=@ְ!@�J@�?}@���@�9X@ӝ�@��H@���@с@�O�@�G�@���@�Z@Ϯ@Ώ\@�M�@��@�/@�Q�@�b@�ƨ@�t�@�C�@ʸR@�@�&�@ȼj@�  @�;d@�~�@�^5@�5?@š�@�?}@ă@�1'@ÍP@�"�@°!@�@�%@��@�I�@��
@��@���@��+@�v�@�V@��@��#@�X@�1'@�dZ@��y@���@�$�@�?}@���@��@�z�@�bN@��u@�(�@�1@��F@��F@�\)@���@��\@�=q@�{@��-@��@�A�@�S�@��y@��y@���@�{@��#@���@���@��T@��T@���@�hs@�&�@�`B@��h@��h@��@���@��@�(�@�  @�  @��m@��F@�l�@�\)@���@��@���@��+@���@���@�n�@���@��@��h@��h@��@�7L@���@���@��u@�Z@�b@��w@��P@�S�@��@���@�n�@�M�@�$�@�X@�1'@��m@�+@�+@��@���@�@��7@��7@�O�@���@��j@�z�@�r�@�j@��;@���@�dZ@�
=@�ff@�{@���@�&�@��@�V@��`@��j@��j@��9@�  @���@��P@��@�t�@�S�@�"�@�^5@�{@��@��#@��T@��@��#@�%@���@���@��@�9X@�1@��m@�dZ@�"�@�@��@���@�~�@�M�@�=q@��@�@��T@���@�x�@�p�@�hs@�O�@��/@��u@���@�
=@��R@��\@�ff@�V@�=q@�$�@���@���@�hs@��D@�I�@�(�@�1@��;@���@�dZ@�
=@�ȴ@���@��\@�v�@�V@�{@���@��h@��@�/@���@�z�@�A�@���@��@��P@�t�@�dZ@�K�@��@��@��@�ȴ@���@�V@��@��^@�x�@�`B@�7L@���@��@�Q�@�9X@�1'@��F@�dZ@�K�@�C�@�+@�o@�
=@���@�~�@�E�@�J@��@���@�@���@�x�@��@��`@���@��@���@��D@�A�@��@���@�t�@�\)@�"�@���@��y@���@�~�@�v�@�M�@��@���@��T@���@��-@��h@�x�@�/@��`@���@�Q�@�b@���@���@��@��@�t�@�;d@���@�ȴ@�v�@�5?@�-@�$�@��#@��-@�x�@�X@�/@�%@��@��9@�j@�A�@�b@�@l�@~$�@}��@}/@|�@|��@|�@{��@{�@{dZ@{@z�!@z=q@z�@y��@yx�@x�u@x1'@v��@v�+@v5?@u��@u/@u/@t��@t9X@sdZ@r�@r�!@rn�@r-@q�@q��@qhs@q&�@p�`@p�@n�R@n5?@n$�@n@m��@mp�@l��@k�F@kS�@ko@k@k@j�@j�@j�@j�H@j��@j=q@i&�@h��@hbN@g�;@g;d@f�R@fv�@e�T@e�h@e?}@dZ@co@b�!@b��@b�\@bM�@b-@b�@a�@aX@`�9@_�P@^�y@^��@^{@]@]�-@]p�@\�/@\��@\�j@\j@[�
@[dZ@[S�@[o@Z��@Z��@Y�@Y&�@Y%@X�u@XbN@W��@W\)@W;d@W
=@V�R@VV@U�-@UO�@T�D@Tj@S�m@St�@R�@Rn�@R-@RJ@Q��@P�u@Pb@O�@O;d@O
=@Nȴ@N$�@M@M@M@Mp�@L��@L��@L�@K�@K"�@J��@J�!@Jn�@J�@I��@I�7@Ix�@IG�@Hb@G�P@GK�@G�@Fȴ@F��@F{@E@E�@E`B@EO�@E�@D�@D9X@CC�@C@B�H@B�H@B~�@A��@@��@@Q�@?�@?�@?�P@?�@>�@>��@>5?@=@=?}@<�@<�D@<(�@;��@;C�@;C�@;@:��@:^5@9��@9x�@9X@9X@9X@97L@8��@8�9@8�u@8�@8r�@8 �@7�w@7K�@7
=@6v�@6E�@65?@6{@5�@5��@5`B@5/@5�@4�j@41@3�F@3S�@3"�@3o@2�H@2��@2n�@2-@1�@1��@1X@1&�@0�9@0�u@0r�@/�;@/��@/\)@.��@.ȴ@.v�@.E�@.5?@.@-@-�h@-�@,�/@,��@,��@,Z@,9X@+��@+ƨ@+�F@+��@+��@+��@+�@+�@+�@+�@+�@+t�@+t�@+C�@+@*��@*=q@*-@*�@)�@)�7@)7L@)%@(�`@(��@(Ĝ@(��@(�@(Q�@(A�@'�@'l�@'+@'
=@&�y@&��@&V@&5?@%�-@%O�@%/@$��@$�/@$��@$�j@$�D@$z�@$z�@$Z@#�m@#dZ@#S�@#C�@#"�@#"�@#o@"�!@"M�@"J@!�^@!hs@!7L@!%@ �`@ ��@ �9@ r�@ Q�@ b@�@�@��@��@|�@l�@�@��@�+@5?@�T@��@�-@��@p�@�@�/@��@Z@(�@�@��@��@33@o@�H@��@^5@=q@�@�^@��@�7@�7@hs@&�@%@Ĝ@Q�@ �@�@;d@�@�R@ff@E�@$�@{@�T@�T@�-@p�@/@�@�@V@��@�@�j@�D@j@Z@9X@1@�F@��@S�@"�@o@@�@�H@�H@��@�\@n�@n�@~�A�
=A�
=A�bA��A� �A�"�A�&�A�&�A�(�A�(�A�$�A�$�A�"�A�"�A�"�A�"�A�"�A�$�A�&�A��A��A��A��A��A��A� �A�"�A�$�A�$�A�&�A�&�A�"�A� �A�"�A�&�A�&�A�+A�-A�(�A�&�A�&�A�&�A�(�A�&�A�&�A�&�A�"�A�$�A�&�A�$�A�"�A�$�A�&�A�+A��AݾwAݺ^AݶFAݴ9AݾwAݼjAݶFA���A���A���A���A���A���A���A���A݅AݍPA�dZA�bNA�9XA�G�A�9XA�$�A��A�JA�A���A���A��A���A��A��A��A��A��yA��HA��;A��/A��#A��A��
A���A�A���AܾwAܾwAܩ�Aܥ�Aܧ�AܮAܣ�AܓuAܑhA܏\AܑhAܗ�Aܗ�AܑhA܏\A�~�A�p�A�l�A�l�A�jA�dZA�^5A�ZA�XA�VA�Q�A�O�A�Q�A�O�A�O�A�M�A�I�A�G�A�G�A�C�A�A�A�?}A�A�A�A�A�=qA�;dA�9XA�9XA�5?A�5?A�33A�5?A�1'A�-A�+A�(�A�(�A�+A�(�A�+A�+A�+A�+A�(�A�&�A�&�A�&�A�$�A�"�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A�{A�oA�oA�oA�oA�{A��A��A��A��A��A�{A�oA�VA�VA�VA�bA�VA�bA�VA�JA�JA�
=A�1A�%A�%A�%A�1A�
=A�
=A�
=A�
=A�
=A�
=A�1A�%A�%A�A�%A�1A�1A�
=A�
=A�1A�A�A�A�%A�%A�1A�1A�1A�%A�%A�  A���A�  A�  A�A�A�  A�A�A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��mA��`A��`A��TA��;A��/A��/A��/A��;A��/A��#A���A�ĜA۸RA۩�Aۣ�Aۛ�Aە�Aۏ\AۋDAۋDAۍPAۇ+AۅA�v�A�v�A�t�A�n�A�ffA�=qA�&�A�bA�7LA�-A�{A��A�{A�oA�bA�JA�VA�JA�JA�JA�JA�JA�
=A�%A�A�A�A�A�A�A�A�bA�oA��A�{A��A��A��A�&�A�7LA�-A�7LA�=qA�=qA�;dA�A�A�C�A�M�A�=qA��A�bA�oA��A��A��A�{A�VA���A��A��yA��#A�ȴAٮA٥�Aٝ�AٓuAه+A�v�A�ffA�G�A��yAة�A�I�A�jA�bAש�A�x�A�I�AָRA�`BA���AՑhA�bNA�M�A�?}A� �A�JA���A��yA��mA��
A��AԑhAԍPA�bNA�jA�r�A�ffA�K�A�M�A�I�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�K�A�I�A�K�A�C�A�5?A�1'A�-A�&�A�{A��A�ĜAӉ7A��Aҧ�A҉7A�l�A�-A�ƨAѬAѣ�Aщ7AыDA�^5A�VA�G�A�oA���A�~�A��A��#A�1'A� �A��A��A��A�{A�%A��;A͙�A�K�A�$�A�VA��A̗�A�"�A�v�A��AʬAɼjA�
=A�r�A���Aǥ�A�G�A�$�A��A�|�A�33A��A�z�A���Aĝ�A�Q�A���AîAÅA�hsA�VA��A�A�;dA�1'A���A��hA�?}A���A��^A��-A��!A���A�jA�Q�A�-A��A���A�n�A���A��RA�n�A�ƨA�VA�1A���A���A��hA��A�|�A�z�A�x�A�x�A�r�A�jA�`BA�9XA��HA�ZA��jA�A��A�M�A��A�
=A�A���A��`A���A��^A���A���A���A��DA�r�A�O�A�1'A���A��
A�ĜA��9A���A��PA�dZA�?}A�=qA�/A��A���A��mA���A���A�v�A�I�A�33A��A�  A��yA��
A�ĜA��-A���A�z�A�"�A���A��
A�ĜA���A�r�A�O�A�C�A�5?A�"�A���A��/A���A��^A���A���A��7A�z�A�r�A�dZA�ZA�XA�Q�A�O�A�I�A�C�A�?}A�9XA�7LA�33A�33A�5?A�33A�/A�$�A��A�{A�bA�
=A�A���A�  A���A��yA��TA�ƨA��jA��FA��9A���A���A��\A��7A��A�x�A�t�A�l�A�jA�hsA�dZA�bNA�^5A�ZA�S�A�Q�A�G�A�G�A�E�A�A�A�9XA�1'A�&�A��A���A��;A�ƨA���A�jA��A��
A���A��FA���A�~�A�n�A�dZA�ZA�Q�A�A�A�/A� �A��A�VA�JA���A��/A���A�ĜA��!A���A��DA�p�A�ZA�?}A��A�1A��A���A���A�n�A�S�A�1'A���A���A��FA���A���A���A��PA��DA��PA�~�A�r�A�hsA�M�A��A�
=A�A�  A���A��mA���A��9A��\A�&�A��yA��9A�?}A�(�A��A�bA�A��yA��A��!A��A�p�A�XA�O�A�E�A�$�A�oA���A��yA��
A�ĜA��^A���A��uA��+A�n�A�G�A�oA��A��wA��DA�ffA�C�A�$�A��A���A��!A���A���A�~�A�^5A��A��yA���A�&�A��A���A�dZA�=qA�-A�{A���A���A��A���A��A�p�A�XA�;dA�bA���A��A��7A�t�A�VA�5?A��A�JA��A��#A�ȴA��A���A�~�A�`BA�C�A�1'A��A�  A��A�ƨA���A��A�dZA�=qA���A��9A��PA�z�A�n�A�\)A�E�A�oA�VA�bA���A��#A��9A���A���A���A��hA��DA��PA��DA��+A�~�A�jA��A��/A���A�ZA�"�A�1A��TA���A��7A�jA�VA�A�z�A�+A��HA���A���A�I�A���A�=qA�JA��A��A��-A�G�A��A�A���A���A�x�A�jA�bNA�^5A�XA�O�A�G�A�E�A�C�A�A�A�;dA�9XA�33A�/A�+A�$�A� �A��A��A��A�{A�VA�%A���A��A���A���A���A��A��A��A��`A��A���A�ƨA��jA��-A���A���A��7A�x�A�bNA�S�A�E�A�5?A��A�JA�  A���A���A���A���A��A��A��#A�`BA��A�G�A���A��uA�+A��!A�z�A�ZA�G�A�?}A�=qA�/A�VA���A��A���A���A��A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                               A��A�$�A�$�A�$�A� �A� �A� �A�"�A�$�A�(�A�(�A�&�A�bA���Aݺ^A�9XA���A��TAܼjAܛ�A�~�A�XA�I�A�=qA�33A�(�A�"�A��A��A��A�{A�{A�VA�1A�1A�1A�A�A���A���A���A���A��A��;A۲-AۅA�oA�oA�1A�1A��A�=qA�{A���A�bA��`A��Aԕ�A�M�A�G�A���A�E�A�M�A���AͲ-A��HA��A���AÙ�A��A���A�z�A�Q�A��A�+A��FA��HA�|�A���A�1A���A�$�A��hA�
=A�ffA�jA���A��A�VA��A��FA�oA��mA��PA�|�A�&�A��TA���A��PA�p�A�G�A�%A��A� �A��\A���A���A�Q�A�;dA�|�A� �A�r�A���A���A��A�ĜA~Q�A|�RA{\)AxI�As��Apr�Aj�/AgAbQ�A_+AZ�AVr�AT��AQ�;AP{AO/AN�AJ��AF��AD^5ACS�AB  A?��A=�A;|�A9�7A7`BA4ĜA2M�A1�wA1p�A0�A/�-A,ĜA+hsA*5?A)�hA)G�A(5?A'�wA'�^A'��A';dA&�A%�A%�A%O�A$A#%A!hsA ��A JA?}A�A�-AhsAC�A�9A^5A�A��AbNA�;A��AS�A�`A  A`BAoAȴAVA��A��A7LA�AA|�A�Ax�A+A%A�9A9XA1A�mA�A��A��A��A�A�!AI�AJA��A�AS�A�/AQ�AJA�A��A��A?}A
�yA
 �AĜA-AG�A+A�A�jA��A��A�uA�+An�A=qA�mAC�A�HA^5A�FA&�A�wA ~�A bNA M�A 9XA J@��@�V@���@���@��@��R@��#@���@��7@�@��u@�1'@��@�@��@�w@�p�@��@���@�9@�I�@�"�@�E�@��@�A�@�b@��H@�^5@���@���@畁@�!@���@�x�@�G�@�x�@��@�x�@�G�@�?}@��`@�z�@�9X@�"�@�n�@���@�X@�&�@�%@��D@��@�"�@ݡ�@ۥ�@��@�J@���@�A�@�  @�|�@�
=@ְ!@�J@�?}@���@�9X@ӝ�@��H@���@с@�O�@�G�@���@�Z@Ϯ@Ώ\@�M�@��@�/@�Q�@�b@�ƨ@�t�@�C�@ʸR@�@�&�@ȼj@�  @�;d@�~�@�^5@�5?@š�@�?}@ă@�1'@ÍP@�"�@°!@�@�%@��@�I�@��
@��@���@��+@�v�@�V@��@��#@�X@�1'@�dZ@��y@���@�$�@�?}@���@��@�z�@�bN@��u@�(�@�1@��F@��F@�\)@���@��\@�=q@�{@��-@��@�A�@�S�@��y@��y@���@�{@��#@���@���@��T@��T@���@�hs@�&�@�`B@��h@��h@��@���@��@�(�@�  @�  @��m@��F@�l�@�\)@���@��@���@��+@���@���@�n�@���@��@��h@��h@��@�7L@���@���@��u@�Z@�b@��w@��P@�S�@��@���@�n�@�M�@�$�@�X@�1'@��m@�+@�+@��@���@�@��7@��7@�O�@���@��j@�z�@�r�@�j@��;@���@�dZ@�
=@�ff@�{@���@�&�@��@�V@��`@��j@��j@��9@�  @���@��P@��@�t�@�S�@�"�@�^5@�{@��@��#@��T@��@��#@�%@���@���@��@�9X@�1@��m@�dZ@�"�@�@��@���@�~�@�M�@�=q@��@�@��T@���@�x�@�p�@�hs@�O�@��/@��u@���@�
=@��R@��\@�ff@�V@�=q@�$�@���@���@�hs@��D@�I�@�(�@�1@��;@���@�dZ@�
=@�ȴ@���@��\@�v�@�V@�{@���@��h@��@�/@���@�z�@�A�@���@��@��P@�t�@�dZ@�K�@��@��@��@�ȴ@���@�V@��@��^@�x�@�`B@�7L@���@��@�Q�@�9X@�1'@��F@�dZ@�K�@�C�@�+@�o@�
=@���@�~�@�E�@�J@��@���@�@���@�x�@��@��`@���@��@���@��D@�A�@��@���@�t�@�\)@�"�@���@��y@���@�~�@�v�@�M�@��@���@��T@���@��-@��h@�x�@�/@��`@���@�Q�@�b@���@���@��@��@�t�@�;d@���@�ȴ@�v�@�5?@�-@�$�@��#@��-@�x�@�X@�/@�%@��@��9@�j@�A�@�b@�@l�@~$�@}��@}/@|�@|��@|�@{��@{�@{dZ@{@z�!@z=q@z�@y��@yx�@x�u@x1'@v��@v�+@v5?@u��@u/@u/@t��@t9X@sdZ@r�@r�!@rn�@r-@q�@q��@qhs@q&�@p�`@p�@n�R@n5?@n$�@n@m��@mp�@l��@k�F@kS�@ko@k@k@j�@j�@j�@j�H@j��@j=q@i&�@h��@hbN@g�;@g;d@f�R@fv�@e�T@e�h@e?}@dZ@co@b�!@b��@b�\@bM�@b-@b�@a�@aX@`�9@_�P@^�y@^��@^{@]@]�-@]p�@\�/@\��@\�j@\j@[�
@[dZ@[S�@[o@Z��@Z��@Y�@Y&�@Y%@X�u@XbN@W��@W\)@W;d@W
=@V�R@VV@U�-@UO�@T�D@Tj@S�m@St�@R�@Rn�@R-@RJ@Q��@P�u@Pb@O�@O;d@O
=@Nȴ@N$�@M@M@M@Mp�@L��@L��@L�@K�@K"�@J��@J�!@Jn�@J�@I��@I�7@Ix�@IG�@Hb@G�P@GK�@G�@Fȴ@F��@F{@E@E�@E`B@EO�@E�@D�@D9X@CC�@C@B�H@B�H@B~�@A��@@��@@Q�@?�@?�@?�P@?�@>�@>��@>5?@=@=?}@<�@<�D@<(�@;��@;C�@;C�@;@:��@:^5@9��@9x�@9X@9X@9X@97L@8��@8�9@8�u@8�@8r�@8 �@7�w@7K�@7
=@6v�@6E�@65?@6{@5�@5��@5`B@5/@5�@4�j@41@3�F@3S�@3"�@3o@2�H@2��@2n�@2-@1�@1��@1X@1&�@0�9@0�u@0r�@/�;@/��@/\)@.��@.ȴ@.v�@.E�@.5?@.@-@-�h@-�@,�/@,��@,��@,Z@,9X@+��@+ƨ@+�F@+��@+��@+��@+�@+�@+�@+�@+�@+t�@+t�@+C�@+@*��@*=q@*-@*�@)�@)�7@)7L@)%@(�`@(��@(Ĝ@(��@(�@(Q�@(A�@'�@'l�@'+@'
=@&�y@&��@&V@&5?@%�-@%O�@%/@$��@$�/@$��@$�j@$�D@$z�@$z�@$Z@#�m@#dZ@#S�@#C�@#"�@#"�@#o@"�!@"M�@"J@!�^@!hs@!7L@!%@ �`@ ��@ �9@ r�@ Q�@ b@�@�@��@��@|�@l�@�@��@�+@5?@�T@��@�-@��@p�@�@�/@��@Z@(�@�@��@��@33@o@�H@��@^5@=q@�@�^@��@�7@�7@hs@&�@%@Ĝ@Q�@ �@�@;d@�@�R@ff@E�@$�@{@�T@�T@�-@p�@/@�@�@V@��@�@�j@�D@j@Z@9X@1@�F@��@S�@"�@o@@�@�H@�H@��@�\@n�@n�G�O�A�
=A�
=A�bA��A� �A�"�A�&�A�&�A�(�A�(�A�$�A�$�A�"�A�"�A�"�A�"�A�"�A�$�A�&�A��A��A��A��A��A��A� �A�"�A�$�A�$�A�&�A�&�A�"�A� �A�"�A�&�A�&�A�+A�-A�(�A�&�A�&�A�&�A�(�A�&�A�&�A�&�A�"�A�$�A�&�A�$�A�"�A�$�A�&�A�+A��AݾwAݺ^AݶFAݴ9AݾwAݼjAݶFA���A���A���A���A���A���A���A���A݅AݍPA�dZA�bNA�9XA�G�A�9XA�$�A��A�JA�A���A���A��A���A��A��A��A��A��yA��HA��;A��/A��#A��A��
A���A�A���AܾwAܾwAܩ�Aܥ�Aܧ�AܮAܣ�AܓuAܑhA܏\AܑhAܗ�Aܗ�AܑhA܏\A�~�A�p�A�l�A�l�A�jA�dZA�^5A�ZA�XA�VA�Q�A�O�A�Q�A�O�A�O�A�M�A�I�A�G�A�G�A�C�A�A�A�?}A�A�A�A�A�=qA�;dA�9XA�9XA�5?A�5?A�33A�5?A�1'A�-A�+A�(�A�(�A�+A�(�A�+A�+A�+A�+A�(�A�&�A�&�A�&�A�$�A�"�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A�{A�oA�oA�oA�oA�{A��A��A��A��A��A�{A�oA�VA�VA�VA�bA�VA�bA�VA�JA�JA�
=A�1A�%A�%A�%A�1A�
=A�
=A�
=A�
=A�
=A�
=A�1A�%A�%A�A�%A�1A�1A�
=A�
=A�1A�A�A�A�%A�%A�1A�1A�1A�%A�%A�  A���A�  A�  A�A�A�  A�A�A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��mA��`A��`A��TA��;A��/A��/A��/A��;A��/A��#A���A�ĜA۸RA۩�Aۣ�Aۛ�Aە�Aۏ\AۋDAۋDAۍPAۇ+AۅA�v�A�v�A�t�A�n�A�ffA�=qA�&�A�bA�7LA�-A�{A��A�{A�oA�bA�JA�VA�JA�JA�JA�JA�JA�
=A�%A�A�A�A�A�A�A�A�bA�oA��A�{A��A��A��A�&�A�7LA�-A�7LA�=qA�=qA�;dA�A�A�C�A�M�A�=qA��A�bA�oA��A��A��A�{A�VA���A��A��yA��#A�ȴAٮA٥�Aٝ�AٓuAه+A�v�A�ffA�G�A��yAة�A�I�A�jA�bAש�A�x�A�I�AָRA�`BA���AՑhA�bNA�M�A�?}A� �A�JA���A��yA��mA��
A��AԑhAԍPA�bNA�jA�r�A�ffA�K�A�M�A�I�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�K�A�I�A�K�A�C�A�5?A�1'A�-A�&�A�{A��A�ĜAӉ7A��Aҧ�A҉7A�l�A�-A�ƨAѬAѣ�Aщ7AыDA�^5A�VA�G�A�oA���A�~�A��A��#A�1'A� �A��A��A��A�{A�%A��;A͙�A�K�A�$�A�VA��A̗�A�"�A�v�A��AʬAɼjA�
=A�r�A���Aǥ�A�G�A�$�A��A�|�A�33A��A�z�A���Aĝ�A�Q�A���AîAÅA�hsA�VA��A�A�;dA�1'A���A��hA�?}A���A��^A��-A��!A���A�jA�Q�A�-A��A���A�n�A���A��RA�n�A�ƨA�VA�1A���A���A��hA��A�|�A�z�A�x�A�x�A�r�A�jA�`BA�9XA��HA�ZA��jA�A��A�M�A��A�
=A�A���A��`A���A��^A���A���A���A��DA�r�A�O�A�1'A���A��
A�ĜA��9A���A��PA�dZA�?}A�=qA�/A��A���A��mA���A���A�v�A�I�A�33A��A�  A��yA��
A�ĜA��-A���A�z�A�"�A���A��
A�ĜA���A�r�A�O�A�C�A�5?A�"�A���A��/A���A��^A���A���A��7A�z�A�r�A�dZA�ZA�XA�Q�A�O�A�I�A�C�A�?}A�9XA�7LA�33A�33A�5?A�33A�/A�$�A��A�{A�bA�
=A�A���A�  A���A��yA��TA�ƨA��jA��FA��9A���A���A��\A��7A��A�x�A�t�A�l�A�jA�hsA�dZA�bNA�^5A�ZA�S�A�Q�A�G�A�G�A�E�A�A�A�9XA�1'A�&�A��A���A��;A�ƨA���A�jA��A��
A���A��FA���A�~�A�n�A�dZA�ZA�Q�A�A�A�/A� �A��A�VA�JA���A��/A���A�ĜA��!A���A��DA�p�A�ZA�?}A��A�1A��A���A���A�n�A�S�A�1'A���A���A��FA���A���A���A��PA��DA��PA�~�A�r�A�hsA�M�A��A�
=A�A�  A���A��mA���A��9A��\A�&�A��yA��9A�?}A�(�A��A�bA�A��yA��A��!A��A�p�A�XA�O�A�E�A�$�A�oA���A��yA��
A�ĜA��^A���A��uA��+A�n�A�G�A�oA��A��wA��DA�ffA�C�A�$�A��A���A��!A���A���A�~�A�^5A��A��yA���A�&�A��A���A�dZA�=qA�-A�{A���A���A��A���A��A�p�A�XA�;dA�bA���A��A��7A�t�A�VA�5?A��A�JA��A��#A�ȴA��A���A�~�A�`BA�C�A�1'A��A�  A��A�ƨA���A��A�dZA�=qA���A��9A��PA�z�A�n�A�\)A�E�A�oA�VA�bA���A��#A��9A���A���A���A��hA��DA��PA��DA��+A�~�A�jA��A��/A���A�ZA�"�A�1A��TA���A��7A�jA�VA�A�z�A�+A��HA���A���A�I�A���A�=qA�JA��A��A��-A�G�A��A�A���A���A�x�A�jA�bNA�^5A�XA�O�A�G�A�E�A�C�A�A�A�;dA�9XA�33A�/A�+A�$�A� �A��A��A��A�{A�VA�%A���A��A���A���A���A��A��A��A��`A��A���A�ƨA��jA��-A���A���A��7A�x�A�bNA�S�A�E�A�5?A��A�JA�  A���A���A���A���A��A��A��#A�`BA��A�G�A���A��uA�+A��!A�z�A�ZA�G�A�?}A�=qA�/A�VA���A��A���A���A��A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                               ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BYB�B�B�B�B�BBSBBSBSBBSB�BoBoB�BVB�B�B�BDB
�B
=B�B
rB
�B
rB
rB
�B
�B
�B
�B
�B
�B
�B
�B
rB
=B
=B
=B
rB4BYB�BIB'�B.IB.IB0UB6�BD3BPHBO�BH�BG�BOvBTaBW
BU�BZBkBf�By�Bs�B�iB�*B͟B�MB�B1B&LBA�B9�B<�BW�BUgBT�BWsBXyBK�BCaB<�B7LB'�BqB(B�B�B�B�B�BɺB��B�wB�dB��B�B��B~�BiyBd�BV�BEmB%zB�B
�B
�9B
�	B
�.B
x8B
VB
>�B
1�B
(�B
#B
�B
�B	��B	��B	��B	͟B	��B	�-B	��B	�B	m)B	_B	U2B	NB	C�B	>wB	:*B	3�B	+6B	%�B	"�B	#�B	'RB	'�B	"hB	�B	CB		B	$B	FB	@B	�B	B	�B	�B	:B	uB	@B	�B	�B	�B	�B	B	=B	B	�B	�B	!�B	'B	/�B	2-B	9�B	CaB	NB	V�B	YKB	Y�B	_pB	`�B	bB	d�B	oiB	s�B	v`B	x8B	�iB	�JB	�bB	��B	� B	�:B	��B	��B	��B	�FB	�7B	��B	��B	�	B	�IB	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�RB	��B	��B	��B	��B	�B	�IB	�wB	��B	��B	�B	��B	�OB	��B	��B	��B	��B	��B	�LB	��B	�B	��B	�zB	�FB	�B	�B	��B	�FB	��B	�?B	�nB	��B	��B	�OB	��B	�IB	�B	��B	�OB	�B	��B	�CB	��B	�wB	�}B	�OB	�3B	�B	�LB	�B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�gB	��B	�B	�gB	��B	�B	��B	��B	�B	ŢB	��B	�aB	ÖB	ĜB	ǮB	��B	�B	�B	�jB	�<B	͟B	�B	�HB	�B	ϫB	��B	��B	�<B	ΥB	��B	ϫB	�}B	уB	ΥB	�B	ѷB	� B	� B	��B	�2B	�gB	��B	��B	�,B	�9B	��B	�9B	��B	�9B	�B	՛B	�mB	՛B	�mB	՛B	՛B	�
B	�B	خB	خB	خB	خB	��B	خB	�KB	خB	�B	�QB	��B	��B	��B	یB	��B	��B	��B	�]B	�dB	��B	�/B	��B	یB	��B	�)B	��B	��B	��B	ٴB	�#B	�#B	ںB	ٴB	ٴB	�#B	��B	یB	چB	�B	چB	ںB	�)B	ݘB	��B	��B	� B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	�cB	�cB	�B	�B	�5B	�;B	�B	�AB	�B	�%B	��B	�lB	�2B	��B	��B	�fB	��B	��B	��B	��B	�2B	��B	�B	�"B	�B	�PB
 �B
uB
uB
uB
uB
�B
MB
�B
%B
�B
�B
+B
_B
1B
	B
�B
�B
�B
fB
�B
_B
�B
1B
�B
�B
_B
+B
�B
1B
�B
�B
�B
	�B
	7B
	�B

	B
	�B
	�B

	B

	B

	B
�B
B
B
JB
�B
�B
PB
B
�B
�B
�B
"B
B
~B
~B
B
�B
�B
PB
�B
PB
�B
�B
�B
�B
�B
\B
\B
\B
bB
 B
B
�B
�B
B
�B
uB
�B
�B
�B
�B
�B
{B
{B
{B
FB
B
�B
B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
7B
kB
�B
�B
�B
�B
�B
�B
B
xB
�B
B
�B
�B
�B
�B
�B
VB
�B
�B
 'B
 'B
 'B
 �B
 �B
 �B
 �B
!bB
!�B
!�B
"�B
"�B
"�B
#B
$B
$tB
$tB
$�B
$B
%�B
%�B
&B
%�B
&B
%�B
%�B
&�B
'B
'RB
'�B
'�B
'�B
'�B
($B
($B
)*B
)_B
)�B
)_B
)_B
)_B
)�B
)�B
)�B
+kB
*�B
+�B
+�B
+�B
,�B
,�B
,qB
-B
-CB
-�B
-CB
-wB
-wB
-�B
-�B
.}B
.�B
/B
/�B
0!B
0�B
0�B
0�B
0�B
0�B
1[B
1�B
1�B
2�B
2�B
2�B
2aB
2-B
1�B
2�B
2�B
2�B
2�B
2�B
33B
3�B
49B
3�B
4B
5B
5�B
5tB
6FB
6B
6FB
6�B
6�B
7B
6�B
7�B
7LB
7�B
7LB
7LB
7LB
8B
7�B
:*B
9�B
:^B
:�B
:�B
:^B
:�B
;0B
<6B
<B
<6B
<6B
<jB
<�B
=B
=B
<�B
<�B
<�B
?}B
>wB
>BB
>B
>wB
>BB
>�B
?HB
?HB
?B
?B
?HB
?B
>�B
>�B
>�B
>�B
>�B
>wB
>wB
>�B
?}B
@B
@OB
@�B
A B
@�B
AUB
B�B
CaB
C�B
C�B
C�B
C�B
CaB
C-B
CaB
C�B
DgB
EB
E�B
E�B
F�B
F�B
F?B
FtB
GEB
GEB
HB
H�B
IB
I�B
IRB
I�B
I�B
I�B
J�B
J#B
JXB
J�B
JXB
K^B
K^B
K^B
K)B
K�B
K�B
LdB
L0B
L�B
L�B
M6B
M�B
NB
NpB
N<B
M�B
N�B
O�B
O�B
PB
PB
PHB
P�B
RTB
S&B
R�B
R�B
R�B
S�B
S�B
T�B
TaB
T�B
T�B
T�B
T�B
UgB
U2B
U2B
T�B
T�B
U�B
V9B
V9B
VmB
V�B
W?B
W�B
W�B
W�B
W�B
W�B
W�B
XB
XEB
Y�B
YKB
YB
YB
Y�B
Z�B
[#B
[�B
[�B
[�B
[�B
\�B
\]B
\�B
\�B
]/B
]/B
]dB
]dB
]�B
^5B
^�B
^5B
^�B
^�B
^�B
_pB
_�B
`B
_�B
_�B
_�B
`B
`vB
`�B
`�B
`vB
`�B
`�B
a|B
a�B
bB
bNB
bNB
b�B
bNB
b�B
b�B
b�B
b�B
c B
c�B
c�B
d&B
dZB
dZB
d�B
d�B
d�B
d�B
d�B
e`B
e�B
e�B
e�B
e�B
f2B
f�B
f�B
gB
gmB
gmB
g�B
h
B
h
B
h
B
hsB
h>B
iDB
iDB
iDB
i�B
i�B
jB
jKB
j�B
j�B
kB
kQB
kQB
kQB
kQB
kQB
kQB
k�B
k�B
k�B
k�B
lWB
l�B
m)B
m)B
m)B
m]B
m�B
m�B
n/B
n/B
n/B
n/B
n/B
ncB
n�B
n�B
oiB
o�B
p;B
p;B
poB
qB
qB
qB
q�B
q�B
q�B
q�B
rGB
q�B
rB
rGB
r|B
rB
rGB
r�B
sMB
sB
r�B
sMB
sB
sB
s�B
s�B
s�B
t�B
tTB
t�B
t�B
t�B
u%B
t�B
u�B
uZB
v+B
u�B
u�B
v+B
u�B
v�B
v+B
v�B
v�B
w2B
w2B
w�B
w2B
w�B
wfB
w�B
w�B
w�B
x8B
x8B
x�B
xlB
x�B
yrB
zB
y�B
y�B
zDB
z�B
zxB
zxB
{B
z�B
{B
z�B
{B
{B
{JB
{�B
|PB
{�B
|�B
}"B
}VB
}�B
}�B
}�B
}�B
~(B
}�B
}�B
~�B
~]B
~�B
~�B
~�B
~�B
~]B
~�B
cB
~�B
cB
�B
~�B
.B
� B
� B
� B
�4B
��B
�4B
��B
�B
�iB
��B
�B
�B
�oB
��BeBeB+B�BYBSBBB�B�B�B�B�B�B�B�BYB�B�B�B$B�B$B�B$BSB�BMBB�B�B�B$B�BB$B{BMBSB�BYB�B�BB�B�BSBB�B$B�B�B�B�B�B�B�B�B�B"B�B�B.B4B$B�B\BhB�BhB�B4B�BhB�B�BbB�B4B�BbB�B\BhBVB�BVB�B�B\B�B\B�B(B\B�B"B�B�BxB
rB�BDBxB�B�B�B�BJBBxB
�BB�B\BJB�B
=B
�B
�BDB
�B�BJBBB
rB	�B	�B
=B
rB
rB
rBxBxBxB	lB	7B
=B	�B
rB
=BxBBB
	BDB�BDBDBB
�B
�B	�B	7B	�B	lB	�B
	B	�B	�B
=B
=BDBDBxBDB�BxB
�B
rB	7B	lB	7B	�B
�B
�B
�B
�BDB
�B
	B	�B	�B	�B
=B
rB
	BBBxB�BxBDB
�B
	B	�B	lB	�B
=B
	BxBB�B
�B
rB
�B	�B	�B
�B
rB
�BBDB�BDB
rB
	B
	B	lB	�B
	B
=B
�BBxB�BDB
�BB	�B	�B
=BxBxB�B
rB	�B	�B	lB	7B	�B
rBDBxB
�B
rB	�B	7B	7B	B	7B	lB	�B
	B
=B
rB
rB
�BBDBB
rB
=B
	B	�B	�B	�B	lB	�B	7B	7B	lB	�B
�B
�BBxBxBDB�B.BbB�B�B4BB�BFBSB�B�B�B�B_B�B�B�BkB�BB7BxBCB�BB�BCBBCBB�B�BOB 'B'B'RB(�BO�B.}B3hB,�B.B-�B.}B.B-B-CB-B-B,�B,�B,�B.IB.�B/B/B/�B/�B0�B0�B/B6B5�B6B5?B5�B2�B1'B2�B@B<�B?B@�BB[BE�BFBMjBS&BN�BMBMBP}BR�BR BQ�BQ�BQ�BNpBPBP}BR BNpBM�BMBLdBL0BJ�BI�BK�BLdBN�BA�B9XBQ�B@�B=�B>�BR BC�BP�BR�BNBL�BPHBQ�BO�BR�BL�BO�BO�BR BV�B]�BZBS�BT�BXyB\�BU�BV�BW�BVBV�BVmBVBV9BVBT�BT�BT,BU2BXEBVBVmBVBWsBXyB[�Ba�Bk�BkQBgBg�Be�Bm�Bh>BffBiBb�Bg�Be�Bf2Bn�BkQBu�BsB��Bw2Br|Bq�BrBp�BrBq�BzxBz�BrBrBl�Bm)Bu�B}�B�B�rB��B��B�XB��B��B�6B�$B�?B�6BBĜB��B�&B�2B�B�B�B�;B�B��B��B��B�cBSBoB�B:B	BqB�B�BYB�BIB�B�B 'B�B.}BK)B)�BB'BN<BE9BA B?B=qB;�B:*B:^B9�B8�B7�B7B6�B6FB:�B;�BR�BPB_�BYBZQBW?BS[BT�BS�BV9BVBT�BUgBT�BT�BT�BU�BV�BU�B\�BVBUgBW?BVmBWsB[�BW�BU�BV�BW�BV�BV�BV9BW�B[WBYBYKBZ�BZBY�BYKBXEBV�BVmBU�B]�B[�BS�BN<BS�BR�BO�BNpBMBN<BQ�BP}BM6BK�BK^BH�BIBI�BH�BHBF�BE�BD�BD3BDgBEBEmBFBEBDgBDgBCaBB�BCaBC-BB�BB�B@�BA�BC�BA�B@OB@�B?�B?�BC-B>wB>�B=�B<�B;�B=B;0B;�B<6B;dB;�B;0B9�B9$B8�B9XB9�B9�B9XB8�B6�B6�B6B8RB6�B7�B7�B8�B6B4�B4�B4�B9XB0�B,�B+kB+�B)�B&�B%�B%�B%�B'RB$@B%�B"4B!�B �B!�B"�BxB 'B �B�BB�B1BB�BYBB@B�B�B�B@BFBoB\B�B�B�B�BDB	�B�BfB�BB
=B�B�BB;B �BoB 4B��B�B�B�8B  B�oB�B�vB� B�B�B�)B�>B�8B�B��B�ZB�B�NB��B�B�5B��BخBچBخB�mB��B�gBӏB�NB�HB��B�vB�XB�B͟BɺB�tB�gB��BĜB��B�zB��B�^B�NB��B�vB�B�^B�zBȀBȀBȴB�B�BȀB�EB��BŢB��B�#BƨB�'B�UB�[B��B�B�BB�B��B��B��B�<B��B��B��B�BB�OB��B�9B��B�}B�B��B�HB��B��B�'B�OB�OB��B�B�[B�3B��B�*B� B�0B��B�B�<B�wB�<B�B��B��B� B�mB�wB��B�HB�B�FB�RB��B��B��B��B�=B�B�*B��B�$B��B�~B��B�=B��B� B|PB��B��Bu�Bu%BtTBt�Bm�Bm�Bl�BkBiyBjBjBi�Bh
Bh
BiyBh>Bg�Bh
BgBe�Be�Bf�Bd�BdZBd&Bd&BcTBc�B_Be�Bc�B]�B\]Bi�BXBT�BU�BT�BS�BR�BQBP�BO�BO�BM�BO�BI�BHKBI�BIBF?BD�BA�BA�B@B?B>wB=B:�BN�B9�B9�B(�B+�B$@B#nB�B�B�BB�BB	7B�BBSB�B�BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                               BVB�B"BpB"BPBjB�BjB�B�B"BBB
�B�BDBzB�B�B�BB�B[B�B
��B�B[B�B�BBB'B[BBB'B'B�B�B�B�B'B	B�B[B�B$&B%�B%�B'8B-�B<�BI�BLJBL0BI7BJ#BMPBN�BO�B[�Bi_BlWBxRBxRB��B��B�:B�9BJB_B*�B=�B4�BBABTFBO�BQ�BW�BW�BE�B<�B5�B4nB#B_B	�B�B�B��B͹B�+B�B�HB��B��B�|B�BB��Bz�Ba�B`BBQBF�B$&BUB
�-B
��B
�SB
��B
� B
X�B
>�B
,�B
#nB
$�B
,B
�B
AB	��B	�jB	׍B	�B	��B	��B	�B	tB	]dB	V�B	K�B	?B	:�B	=�B	9�B	*�B	!�B	�B	#�B	&fB	&�B	!|B	jB	/B		B	 B	6B	B	�B	�B	:B	�B	0B	�B	�B	�B	PB	6B	B	pB	�B	�B	B	]B	�B	$�B	)�B	,�B	49B	=�B	H�B	O�B	Q�B	S[B	X_B	Y�B	[�B	`�B	h�B	l=B	oB	q�B	{dB	�B	�B	�lB	�rB	��B	��B	�PB	�BB	��B	��B	��B	� B	�B	��B	�sB	�kB	�xB	�dB	��B	��B	�:B	�hB	��B	� B	�bB	�BB	�NB	�HB	�\B	��B	�B	�B	��B	��B	�fB	��B	��B	�6B	��B	��B	�B	�B	�iB	��B	��B	� B	��B	�cB	�}B	��B	��B	�;B	��B	��B	�!B	�iB	�B	��B	��B	��B	��B	��B	�*B	�sB	�8B	��B	�mB	�8B	�8B	��B	��B	��B	�AB	�;B	��B	��B	��B	�B	�B	�DB	�B	�RB	�	B	�B	�BB	�]B	��B	��B	�(B	��B	��B	�(B	��B	�}B	�]B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�?B	ȀB	ɺB	�RB	ȀB	�B	�B	�EB	�1B	�RB	˒B	��B	�DB	�B	��B	��B	ʌB	�)B	��B	��B	��B	�\B	̈́B	��B	ϫB	ϑB	�B	ϫB	�VB	��B	�VB	��B	�BB	бB	�"B	�pB	��B	��B	�B	�4B	�4B	�B	�4B	��B	ңB	ѷB	�&B	��B	�MB	�B	�B	ԯB	��B	�2B	ԯB	՛B	�9B	��B	�YB	�sB	ԕB	�,B	�2B	��B	� B	��B	ѝB	�&B	�uB	�@B	�&B	�aB	��B	ԯB	�,B	өB	��B	��B	ҽB	��B	�gB	�gB	�IB	�=B	�CB	�qB	�;B	�B	�B	�B	��B	�B	�B	�B	�B	�fB	�B	�B	�B	�B	�2B	�2B	��B	�B	�B	�XB	�_B	�B	�WB	��B	�'B	�B	�B	�OB	�OB	�B	��B	��B	�B	��B	��B	�3B	��B	�B	��B	��B	�xB	�0B	��B	��B	�B	�B	��B	�BB	�wB	��B	�.B	��B
 iB
B
 �B
 �B
 �B
 �B	��B	�cB
 �B
�B	�B
 iB	�B	��B	��B
;B
  B	��B
 �B
�B
UB
�B
�B
�B
AB
B
'B
[B
�B
SB
�B
�B
{B
�B
9B
B
�B
SB
�B
tB
�B
3B
3B
�B
�B
�B
mB
�B
B
B
B
�B
�B
�B
zB
EB
�B
fB
	B

�B

�B

�B

�B

�B
�B
�B
~B
�B
~B
�B
�B
dB
0B
�B
�B
dB
~B
�B
�B
pB
"B
pB
VB
VB
pB
�B
�B
\B
 B
�B
�B
�B
B
TB
�B
&B
�B
�B
�B
uB
�B
FB
�B
�B
�B
MB
�B
�B
�B
�B
�B
�B
�B
�B
�B
+B
�B
�B
�B
�B
�B
�B
7B
�B
�B
�B
�B
CB
xB
CB
xB
�B
B
~B
�B
�B
�B
�B
5B
B
VB
pB
�B
�B
�B
�B
 BB
 �B
!HB
!HB
!|B
!B
!HB
!�B
!�B
!�B
"�B
#TB
#B
#�B
#�B
$&B
$�B
$tB
$tB
%,B
%,B
%zB
%B
%`B
%`B
%�B
&B
&�B
'8B
'�B
(>B
(XB
)B
(�B
(�B
(�B
)*B
)�B
)�B
*eB
+B
*B
*eB
*�B
*KB
*0B
*�B
*�B
*�B
*�B
+B
+�B
+�B
,WB
+�B
,qB
.B
-�B
-�B
./B
./B
.}B
.}B
.�B
.�B
.�B
/�B
/iB
/�B
/OB
/iB
0B
0UB
0�B
2GB
2B
2|B
2�B
2|B
2�B
3hB
3�B
4TB
4B
4B
49B
4TB
4�B
4�B
4�B
4�B
5?B
6zB
7�B
6+B
6B
5�B
6�B
6�B
7�B
7fB
72B
6�B
6�B
6�B
6�B
6zB
6�B
6�B
7LB
7�B
6�B
6�B
6�B
7�B
8lB
8RB
8�B
9$B
9$B
:DB
<B
;B
;B
;JB
;�B
;dB
;0B
;0B
;�B
<6B
=qB
=qB
=�B
>(B
>�B
>wB
>BB
>�B
>�B
?B
@4B
AB
A;B
A;B
AUB
AoB
A�B
B'B
CGB
BB
B�B
B�B
B�B
C{B
C-B
CGB
CGB
C�B
DgB
D�B
D�B
D�B
D�B
EmB
FB
F?B
FYB
F%B
F%B
GzB
H1B
G�B
HKB
G�B
HKB
IB
JXB
J�B
J�B
J�B
K)B
K�B
L~B
MB
L~B
L�B
L�B
L�B
MB
M�B
MB
L�B
MB
M�B
M�B
NB
NB
NVB
N�B
OBB
O�B
OvB
O�B
O�B
O�B
O�B
PB
P�B
Q�B
Q B
QB
Q B
RB
R�B
S@B
SuB
S�B
S�B
S�B
TaB
T,B
T�B
T�B
U2B
UB
UMB
UMB
U�B
VB
V9B
VB
VmB
V�B
V�B
WsB
W�B
W�B
WsB
W�B
W�B
W�B
X+B
X_B
X_B
X_B
X�B
X�B
YKB
Y�B
Y�B
ZB
ZB
Z7B
Z7B
Z�B
Z�B
ZkB
Z�B
[qB
[�B
[�B
[�B
\B
\)B
\xB
\�B
\�B
\�B
\�B
]IB
]dB
]�B
]�B
]�B
^jB
^�B
^�B
_B
_;B
_VB
_�B
_�B
_�B
_�B
`BB
`\B
aB
`�B
aB
a�B
a�B
a�B
bB
bhB
b�B
b�B
b�B
cB
b�B
b�B
b�B
b�B
c:B
c B
cTB
c�B
dZB
d�B
d�B
d�B
d�B
e`B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f2B
fLB
f�B
g�B
g�B
g�B
h
B
hXB
h�B
h�B
i*B
i�B
iyB
iyB
i�B
i�B
i�B
i�B
i�B
jB
i�B
jeB
kB
kB
j�B
j�B
j�B
j�B
k6B
k�B
k�B
k�B
l�B
l"B
l�B
lqB
l�B
l�B
l�B
m�B
mCB
m�B
m]B
m�B
m�B
m�B
n}B
n/B
n�B
n�B
o5B
o5B
oOB
o B
o�B
oOB
o�B
o�B
o�B
p!B
pB
pUB
p;B
p�B
q�B
q�B
q�B
qvB
rGB
r�B
rGB
r|B
s3B
r�B
sB
r�B
r�B
sMB
s3B
s�B
t9B
tB
t�B
t�B
utB
u�B
uZB
utB
u�B
u�B
u�B
u�B
vzB
vFB
v�B
v`B
vFB
v�B
vB
v`B
w2B
vzB
wB
wfB
v�B
wB
w�B
xB
w�B
w�B
x�B
w�B
xRB
x�B
xB
x�B
x�B
x�B
x�G�O�B�B�B�B\B�B�B~B�BPBPB�BVB�B\B�B�B�B�BB�B�B"B�B"B�B�B�B�B~BJBB"B�BVB�B�B�B�B�B"B�B�BPB�BJB�B�B�B�B�B"B�B�B
=B(B$B	B
=B
=B�B
	BJB�B�B�B1B�B�B	B�B	B�BB�BB1B�B1B�B%B�B�B�B�B�B�B�BB�B�B�B�BYB�B�B�B�BBGB�B�B	7B�B�BBMBBMB�BuB�BB{BB�B�BGB�BBB�BABGB�B{BuB�BB;B�B�B�B�B�B�B�B �B �B�BB�B�B�BuBuBoB�BGB�B�BuBBBB �BB �B;BoB;BB�B�B�B�B�B�BB�BB�B �B �B �B;BBABABAB�BBoB;B;BB�B�BoBuBuB�BB�B�BBoB;B �B;B�BoB�B{BBAB�BB;BBB�BBuB�BB�B�BoBoB �BBoB�BBuB�BGB�BABuB;B;B�B�B�BB�B;BB �B �B;B�B�B�BB�BB �B �B iB �B �BBoB�B�B�BABuB�BuB�B�BoB;BBB �BB �B �B �B;BABABuB�B�B�BGB�B�B�B�B�B	lBDB�B�B�B.B.B�B�B\B\B4B�B BhB�B�B�BFB�BB�B{B�B�BMBB�B�B�B�B \BGEB%�B*�B$@B%zB%FB%�B%zB$tB$�B$tB$tB$@B$B$@B%�B&B&�B&�B'RB'RB'�B'�B&�B-wB-B-wB,�B-B*0B(�B)�B7�B49B6zB8RB9�B=<B=qBD�BJ�BF?BDgBDgBG�BJ#BI�BIBIRBH�BE�BGzBG�BI�BE�BE9BDgBC�BC�BA�B@�BC-BC�BF?B9XB0�BIB7�B5B6BI�B;0BHKBJ#BEmBD3BG�BIRBGEBJ#BC�BGBGEBI�BN<BU2BQ�BK^BLdBO�BS�BMBNBOBMjBNBM�BMjBM�BMjBL0BLdBK�BL�BO�BMjBM�BMjBN�BO�BR�BYKBcTBb�B^jB_;B]dBd�B_�B]�B`vBZB_B]dB]�Be�Bb�Bm)BjB��Bn�Bi�BiBiyBh>BiyBiDBq�BrGBiyBiyBdZBd�Bl�Bu%ByrB��B�B�PB��B�1B�:B��B��B��B��B��B�B�<BʌB�~B�MB�bB��B�B��B�0B�?B��B��B��B��BB	�BTB�B
BB�B�B�B�B�BsB�B%�BBuB!B9rBE�B<�B8lB6`B4�B3B1vB1�B0�B0;B/ B.cB-�B-�B1�B3BJ	BG_BW$BPbBQ�BN�BJ�BK�BKDBM�BMPBLJBL�BLJBK�BK�BL�BM�BL�BTBMPBL�BN�BM�BN�BS@BO(BMBN"BO(BM�BN"BM�BN�BR�BP�BP�BRBQhBQ BP�BO�BM�BM�BMBT�BSBKDBE�BKBJ	BG+BE�BDMBE�BH�BG�BD�BCGBB�B@ B@iB@�B@ B?cB=�B<�B;�B;B;�B<PB<�B=VB<PB;�B;�B:�B:DB:�B:xB9�B:B88B8�B;B9	B7�B88B6�B72B:xB5�B5�B4�B4B2�B4TB2|B3B3�B2�B3B2|B1AB0oB0;B0�B0�B1B0�B/�B-�B./B-]B/�B./B.�B.�B/�B-]B+�B,"B+�B0�B(
B#�B"�B"�B �B�B/B/B�B�B�B�BB�BEB�BB�BsBEB�BaB�B}BNBB�BdB
�B	�B6B	B
�B�B	�B�B3B9BB�B�B �B�B��B BgB�B�"B�JB�rB��B�8B��B��B��B B�iB�B�fB��B�B��B�fB�mB�|B�BߤBޞB�B�)B��B�BٴB�EB�mB՛B�TB�B��B�B��B�0B��B��BȴBǮB�EB��B��B�}B�B� B��B��B�^B�B�0B��B�*B��BȴB�'B��B�mB��B��B��B��B�B��B��B��B��B�<B�B�'B��B�B��B��B��B�$B�zB��B�tB�B�B�LB��B�?B�B�B�zB��B��B�B��B�RB��B�nB�3B��B�aB�B��B��B��B�$B�wB��B��B��B��B��B��B��B�hB��B��B��B�nB�B�B��B��B��B�XB��B�hB��B��B��B�B�6B�B��B�tB��B�+B��B�B��B�+B��B~(BwfBs�B}VB{�Bm]Bl�Bk�Bk�Be,Bd�Bd&Bb�B`�Ba|Ba�BaB_pB_pB`�B_�B_;B_pB^jB\�B]/B^B\)B[�B[�B[�BZ�BZ�BVmB]/B[WBT�BS�BaHBOvBLdBMBL0BK)BI�BH�BHBGBGEBEBGEBA B?�BA B@�B=�B<B9$B9XB7�B6zB5�B4nB1�BFB1'B1'B�B#B�B�BJB+BGB�B 4BuB �B
�"B
��B
��B
�DB
��B
�lG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                               <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<U��<5[N<#�
<#�
<#�
<#�
<8��<#�
<t֋<#�
<`|�<��<pT�<c�Z<.�<#�
<#�
<`�<#�
<#�
<t<#�
<#�
<#�
<'�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<H25<#�
<#�
<#�
<#�
<0d<#�
<#�
<��@<#�
<#�
<�1Z<�,`<E��<#�
<#�
<#�
<7~9<#�
<#�
<C�<�j<_S�<���<m��<�~�<L#Q<{J�<���<#�
<1$O<#�
<#�
<#�
<Q8<uB�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(�<#�
<#�
<#�
<#�
<#�
<3��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT; PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           NO correction for Conductivity Thermal Mass (CTM) is applied;          PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment; OWC V3.0: r =0.9998(+/-0.0001), vertically averaged dS =-0.0082(+/-0.0047)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     NO correction for Conductivity Thermal Mass (CTM) is applied;    OWC V3.0: r =0.9998(+/-0.0001), vertically averaged dS =-0.0082(+/-0.0047)                                                                                                                     SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:6/3.0,y:3.0/1.0]; Fit Theta<2.5C; max_breaks=1; Prescribed break at cycle 110;                            PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:6/3.0,y:3.0/1.0]; Fit Theta<2.5C; max_breaks=1; Prescribed break at cycle 110;                            PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261924182023042619241820230426192418202304261924182023042619241820230426192418SI  SI  ARFMARFM                                                                                                                                                2022092513061820220925130618IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARCAARCAADJSADJS                                                                                                                                        2022100512012220221005120122  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022100512012220221005120122QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022100512012220221005120122QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�4000            4000            SI  SI  ARFMARFM                                                                                                                                                2023042610040820230426100408IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619242520230426192425IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619242520230426192425IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619242520230426192425IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                